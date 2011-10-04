#!/opt/perl/bin/perl
#
# Copyright (c) 2009 Andreas Jobs and Robin Schröder. All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the Perl Artistic License or the
# GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# If you do not have a copy of the GNU General Public License write to
# the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
# MA 02139, USA.
#
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use YAML;
use AnyEvent;
use AnyEvent::XMPP::IM::Connection;
use AnyEvent::XMPP::Namespaces;
use AnyEvent::XMPP::Ext::Disco;
use AnyEvent::XMPP::Ext::MUC;
use Nagios::Status::ServiceStatus;
use Nagios::Status::HostStatus;
use Text::ParseWords;
#use Data::Dumper;
use Sys::CpuLoad;

use vars qw($event $timer_event $io_event $connection $disco $muc $terminating);

use vars qw($jid $rooms $jids %default %bot_command $config $config_locations $VERSION $configfile $verbose $password);
$|=1;
$VERSION = '0.8';
$verbose = 1;
$terminating = 0;
$config_locations = '.,/etc,/usr/local/etc,/opt/local/etc,/opt/nagios/etc,/usr/local/nagios/etc';
%default = (
    ack_sticky             => 1,
    ack_notify             => 1,
    ack_persistent         => 0,    
    bot_resource           => 'nagibot.pl',
    bot_show_idle_after    => 5,
    bot_status             => 'available',
    bot_status_priority    => 0,
    bot_status_text        => 'Listening',
    nagios_cmd_file        => '/usr/local/nagios/var/rw/nagios.cmd',
    nagios_msg_fifo        => '/usr/local/nagios/var/rw/nagibot.fifo',
    nagios_status_log_file => '/usr/local/nagios/var/status.dat',
);

%bot_command = (
    ack         => \&AcknowledgeNagiosProblem,
    acknowledge => \&AcknowledgeNagiosProblem,
    details     => \&ReportNagiosStatusDetails,
    help        => \&ReportHelp,
    info        => \&ReportInfo,
    status      => \&ReportNagiosStatus,
);

#--- commandline options
Getopt::Long::Configure('bundling');
GetOptions (
    'verbose|v'    => sub { $verbose ++ }, 
    'quiet|q'      => sub { $verbose = 0 },
    'version|V'    => sub { print "$VERSION\n"; exit 0; },
    'config|c=s'   => \$configfile,
    'password|p=s' => \$password,
    'help|h|?'     => sub { pod2usage(-verbose => 1) },
    'man'          => sub { pod2usage(-exitstatus => 0, -verbose => 2) },
) or pod2usage(-verbose => 0);
$jid = shift;

pod2usage(-verbose => 0, -message => 'Cannot start without a JID') unless $jid;

print "nagibot $VERSION starting ...\n" if $verbose;


#--- find configuration
if ($configfile) {
    print "using config file from commandline\n" if $verbose > 2;
    unless (-f $configfile) {
        print "Configuration file $configfile not found.\n";
        exit 1;
    }
}
else {
    print "trying to find configuration file\n" if $verbose > 2;
    foreach (split(',', $config_locations)) {
        my $f = $_ . '/nagibot.conf';
        if (-f $f) {
            $configfile = $f;
            last;
        }
    }
}


#--- read configuration
($rooms, $jids, $config) = YAML::LoadFile($configfile);
foreach my $k (keys %default) {
    $config->{$k} = $default{$k} unless defined $config->{$k};
}


#--- override configuration values with commandline parameters
#--- and adjust some values for internal use
$config->{'password'}             = $password if $password;
$config->{'bot_show_idle_after'} *= 60;


#--- install signal handler
$SIG{INT}  = \&SignalHandler;
$SIG{TERM} = \&SignalHandler;
$SIG{HUP}  = \&SignalHandler;

# Add namespace for showing idle time
AnyEvent::XMPP::Namespaces::set_xmpp_ns_alias('last', 'jabber:iq:last');

CON:
$event = AnyEvent->condvar;

$connection = AnyEvent::XMPP::IM::Connection->new (
    jid => $jid,
    password => $config->{'password'},
    resource => $config->{'bot_resource'},
    initial_presence => 0,
);

$connection->reg_cb (
    # On Connect
    session_ready => sub {
        my ($con) = @_;
        print 'Connected as ' . $con->jid . "\n" if $verbose;

        print "Setting presence ...\n" if $verbose > 1;
        $con->send_presence (
            undef,
            undef,
            show => $config->{'bot_status'},
            status => $config->{'bot_status_text'},
            priority => $config->{'bot_status_priority'}
        );

        print "Joining rooms (if any) ...\n" if $verbose > 1;
        $con->add_extension ($disco = AnyEvent::XMPP::Ext::Disco->new);
        $con->add_extension (
            $muc = AnyEvent::XMPP::Ext::MUC->new (disco => $disco, connection => $con)
        );
        for my $room (keys %$rooms) {
            $muc->join_room (
                $con,
                $room,
                $rooms->{$room},
                timeout=>20,
                create_instant => 0,
                history => {stanzas => 0}
            );
        }

        print "Register room message handler ...\n" if $verbose > 1;
        $muc->reg_cb (
            message => sub {
                my ($con, $room, $msg, $is_echo) = @_;

                return if $is_echo;

                print "Message (" . $room->jid .") from " . $msg->from . ": " . $msg->any_body . "\n" if $verbose > 2;
                return unless $msg->any_body =~ /^\@/;
                my $cmd = $1 if $msg->any_body =~ /^\@nagibot (.*)$/i;
                return unless $cmd;
                &SetIdlePresence(0);
                &SendRoomMessage($room, &ProcessBotCommand ($msg->from, $cmd));
                &StartTimer($config->{'bot_show_idle_after'});
            }

        );

        &StartTimer($config->{'bot_show_idle_after'});
    },

    # On Receive
    message => sub {
        my ($con, $msg) = @_;

        return unless $msg->any_body;

        my $from = $con->get_roster()->get_contact($msg->from);

        print "Message from " , $msg->from , "(", $from->is_on_roster() ? "" : "NOT " , "on roster): " , $msg->any_body , "\n" if $verbose > 2;
        return unless $from->is_on_roster;

        &SetIdlePresence(0);
        my $reply = $msg->make_reply;
        $reply->add_body (&ProcessBotCommand($msg->from, $msg->any_body) => undef);
        $reply->send($con);
        &StartTimer($config->{'bot_show_idle_after'});
    },

    # On Error
    error => sub {
        my ($con, $error) = @_;

        warn "Error: " .  $error->string . "\n";
    },
    # On connect
    connect => sub {
        my ($con, $h, $p) = @_;
        print "connected to $h:$p\n" if $verbose > 1;
    },

    # On Disconnect
    disconnect => sub {
        my ($con, $h, $p, $reason) = @_;
        print "Disconnected from $h:$p: $reason\n";
        $event->send;
    },

);

print "Trying to connect ...\n" if $verbose;
$connection->connect ();

print "Opening fifo for reading ...\n" if $verbose;
open (FIFO, "+< " . $config->{'nagios_msg_fifo'}) or die "Cannot open fifo " . $config->{'nagios_msg_fifo'} .": $!";


$io_event = AnyEvent->io (
    fh => \*FIFO, 
    poll => 'r',
    cb => sub {
        return unless $connection->is_connected();

        chomp (my $input = <FIFO>);
        print "read: $input\n" if $verbose > 2;

        # Reset idle presence
        &SetIdlePresence(0);

        # Send message to all rooms
        foreach my $room (keys %$rooms) {
            my $r = $muc->get_room($connection, $room);
            unless ($r) {
                print "ERROR: Lost connection to the room $room";
                # TODO: Implement rejoin
                return;
            }
            my $msg =  $r->make_message (body => $input);
            $msg->send ();
        }

        # send message to all users
        foreach my $ujid (@$jids) {
            my $msg =  AnyEvent::XMPP::IM::Message->new (to => $ujid, body => $input, type => 'chat');
            $msg->send ($connection);
        }

        # Restart idle timer
        &StartTimer($config->{'bot_show_idle_after'});
    }
);

$event->wait;

undef $io_event;
undef $timer_event;
undef $connection;

# reconnect ?
goto CON unless ($terminating);

print "Quitting.\n" if $verbose;

exit 0;


sub SignalHandler {
    my $sig = shift;

    print "Got $sig signal. Terminating.\n" if $verbose;
    $terminating = 1;
    $connection->disconnect("$sig signal") if $connection;
}

sub StartTimer {
    my $seconds = shift;

    undef $timer_event if $timer_event;
    return unless $seconds > 0;

    print "Starting idle timer ($seconds seconds).\n"if $verbose > 2;
    $timer_event = AnyEvent->timer (
        after => $seconds,
        cb    => sub {
            &SetIdlePresence($seconds);
        },
    );
}

sub SetIdlePresence {
    my $idle = shift;

    undef $timer_event if $timer_event and !$idle;

    print "Sending " , $idle ? "idle ": "", "presence ...\n" if $verbose > 2;
    $connection->send_presence (
        undef,
        $idle ? 
        {
            defns => 'last',
            node  => {
                name  => 'query',
                attrs => [ 'seconds', $idle ],
            },
        } : undef,
    );
}

sub SendRoomMessage {
    my ($room, $msg) = @_;

    my $mmsg =  $room->make_message (body => $msg);
    $mmsg->send ();
}


sub ProcessBotCommand {
    my $from_jid = $_[0];
    my ($cmd, $param) = split(' ', $_[1], 2);

    return unless $cmd;

    $cmd = lc $cmd;
    return "What?\nType 'help' for more info!" unless defined $bot_command{$cmd};
    return $bot_command{$cmd}($from_jid, $param);
}


sub ReportHelp {
    my $msg = "NagiBot $VERSION Help\n";
    $msg .= "Available commands:\n";
    $msg .= "  status\t- basic status information.\n";
    $msg .= "  details\t- detailed information about anything that is not okay.\n";
    $msg .= "  info\t- basic system info\n";
    $msg .= "  ack [<service>@]<host> <comment>\n\t\t- acknowledge service/host problem\n"; 
    $msg .= "  help\t- you just typed 'help', didn't you?";

    return $msg;
}


sub ReportNagiosStatus {
    use vars qw( $status $ok $cr $un $wa $up $dn $msg);

    $msg = "";

    $status = Nagios::Status::HostStatus->new($config->{'nagios_status_log_file'});
    if ($status) {
        $up = $status->check_up;
        $dn = $status->check_down;
        $un = $status->check_unreachable;

        $msg .= scalar @$up . " host" . (scalar @$up == 1 ? "" : "s") . " UP";
        $msg .= ", " . scalar @$dn . " host" . (scalar @$dn == 1 ? "" : "s") . " DOWN" if $dn && @$dn;
        $msg .= ", " . scalar @$un . " host" . (scalar @$un == 1 ? "" : "s") . " UNKNOWN" if  $un && @$un;
    }
    else {
        $msg .= "ERROR: Cannot create Nagios::Status::HostStatus object";
    }
    $msg .= "\n";

    $status = Nagios::Status::ServiceStatus->new($config->{'nagios_status_log_file'});
    if ($status) {
        $ok = $status->check_ok;
        $cr = $status->check_critical;
        $un = $status->check_unknown;
        $wa = $status->check_warning;

        $msg .= scalar @$ok . " service" . (scalar @$ok == 1 ? "" : "s") . " OK";
        $msg .= ", " . scalar @$wa . " service" . (scalar @$wa == 1 ? "" : "s") . " WARNING" if $wa && @$wa;
        $msg .= ", " . scalar @$cr . " service" . (scalar @$cr == 1 ? "" : "s") . " CRITICAL" if $cr && @$cr;
        $msg .= ", " . scalar @$un . " service" . (scalar @$un == 1 ? "" : "s") . " UNKNOWN" if  $un && @$un;
    }
    else {
        $msg .= "ERROR: Cannot create Nagios::Status::ServiceStatus object";
    }
    return $msg;
}


sub ReportNagiosStatusDetails {
    use vars qw( $service_status $host_status @lines );

    @lines = ();

    $host_status = Nagios::Status::HostStatus->new($config->{'nagios_status_log_file'});
    if ($host_status) {
        my $dn = $host_status->check_down;
        my $un = $host_status->check_unreachable;

        my $status;
        push @lines, "Nagios Host Details";
        if ($dn or $un) {
            foreach my $s ("DOWN",@$dn, "UNREACHABLE",@$un) {
                next unless defined $s;
                $status = $s, next unless ref($s);

                my $line = $s->get_hostname . " " . $status;

                push @lines, $line;
            }
        } else {
            push @lines, "All hosts up";
        }
    }
    else {
        push @lines, "ERROR: Cannot create Nagios::Status::HostStatus object";
    }

    $service_status = Nagios::Status::ServiceStatus->new($config->{'nagios_status_log_file'});
    if ($service_status) {
        my $cr = $service_status->check_critical;
        my $wa = $service_status->check_warning;
        my $un = $service_status->check_unknown;

        my $status;
        push @lines, "Nagios Service Details";
        if ($cr or $wa or $un) {
            foreach my $s ("CRITICAL",@$cr, "WARNING",@$wa, "UNKNOWN",@$un) {
                next unless defined $s;
                $status = $s, next unless ref($s);

                my $line = $s->get_hostname . " " . $s->get_attribute('service_description') . " " . $status;
                $line .= " (ACK)" if $s->get_attribute('problem_has_been_acknowledged');
                $line .= "\n   " . $s->get_attribute('plugin_output');

                push @lines, $line;
            }
        } else {
            push @lines, "All services OK";
        }
    }
    else {
        push @lines, "ERROR: Cannot create Nagios::Status::ServiceStatus object";
    }
    return join("\n", @lines);
}


sub ReportInfo {
    my $msg;
    $msg .= "This is nagibot $VERSION\nSystem Info:\n";
    $msg .= 'Current Load (1,5,15 min): ' . join(' ', Sys::CpuLoad::load());

    return $msg;
}

sub AcknowledgeNagiosProblem {
    my $from_jid = shift @_;
    my @params = shellwords(@_);
    my ($host, $service, $comment) = undef;
    my $msg = 'Something weird happened';

    # Syntax:  ack service@host comment
    #   OR  
    # Syntax:  ack host comment
    if ($params[0] =~ /\@/) {
        ($service, $host) = split('@', $params[0]);
    }
    else {
        $host = $params[0];
    }
    shift @params;
    $comment = join (' ', @params);

    if ($service) {
        my $status = Nagios::Status::ServiceStatus->new($config->{'nagios_status_log_file'});
        if ($status) {
            $msg = '';
            my $cr = $status->check_critical;
            my $wa = $status->check_warning;
            my $un = $status->check_unknown;
            my $ok = $status->check_ok;
            foreach my $s (@$cr, @$wa, @$un, @$ok) {
                next unless $s->get_hostname eq $host;
                next unless $s->get_attribute('service_description') eq $service;

                if ($s->get_attribute('current_state') == 0) {
                    $msg = 'Service is OK. Nothing to acknowledge.';
                }
                elsif ($s->get_attribute('problem_has_been_acknowledged')) {
                    $msg = 'Problem has already been acknowledged.';
                }
                else {
                    my $ack = join (';', 
                        'ACKNOWLEDGE_SVC_PROBLEM',
                        $host,
                        $service,
                        $config->{'ack_sticky'},
                        $config->{'ack_notify'},
                        $config->{'ack_persistent'},
                        $from_jid,
                        $comment
                    );
                    $msg = &WriteExternalNagiosCommand($ack);
                }
            }
            $msg = 'Service not found' unless $msg;
        }
        else {
            $msg .= "ERROR: Cannot create Nagios::Status::ServiceStatus object";
        }
    }
    else {
        # do the same for a host ack
        my $host_status = Nagios::Status::HostStatus->new($config->{'nagios_status_log_file'});
        if ($host_status) {
            $msg = '';
            my $dn = $host_status->check_down;
            my $un = $host_status->check_unreachable;
            my $up = $host_status->check_up;
            foreach my $s (@$dn, @$un, @$up) {
                next unless $s->get_hostname eq $host;

                if ($s->get_attribute('current_state') == 0) {
                    $msg = 'Host is UP. Nothing to acknowledge.';
                }
                elsif ($s->get_attribute('problem_has_been_acknowledged')) {
                    $msg = 'Problem has already been acknowledged.';
                }
                else {
                    my $ack = join (';', 
                        'ACKNOWLEDGE_HOST_PROBLEM', 
                        $host,
                        $service,
                        $config->{'ack_sticky'},
                        $config->{'ack_notify'},
                        $config->{'ack_persistent'},
                        $from_jid,
                        $comment
                    );
                    $msg = &WriteExternalNagiosCommand($ack);
                }
            }
            $msg = 'Host not found' unless $msg;
        }
        else {
            $msg .= "ERROR: Cannot create Nagios::Status::HostStatus object";
        }
    }

    return $msg;
}

sub WriteExternalNagiosCommand {
    my $ack_msg = shift;

    open (C, '>>' . $config->{'nagios_cmd_file'}) or return 'Cannot open Nagios command file for writing';
    print C '[', time ,'] ', $ack_msg, "\n";
    print 'sending nagios cmd: [', time ,'] ', $ack_msg, "\n" if $verbose > 2;
    close (C);
    return 'OK';
}

1;

__END__

=head1 NAME

nagibot.pl - A XMPP robot for Nagios(tm) notifications and interaction.

=head1 SYNOPSIS

nagibot.pl [options] JID

=head1 DESCRIPTION

B<This program> acts as a XMPP bot and forwards Nagios(tm) notifications to
XMPP destinations. XMPP destinations may be one or more JIDs or one or more
XMPP chatrooms.

The Nagios(tm) process writes the notifications to a pipe where the bot reads
and processes them. The requirements are shown in L<"PREREQUISITES">.

You can also send messages to the bot (either directly or in a multi user
chatroom). The bot accepts a few commands as shown in L<"ACCEPTED COMMANDS">.

=head1 OPTIONS

=over 8

=item B<-?>, B<-h>, B<--help>

Print a brief help message and exit.

=item B<--man>

Print the manual page and exit.

=item B<-V>, B<--version>

Print version number and exit.

=item B<-v>, B<--verbose>

Be verbose. You can specify this more than once to increase verbosity.

=item B<-q>, B<--quiet>

Set verbosity to zero. Error messages are printed but no warnings or
informational messages will be shown.

=item B<-c> I<FILE>, B<--config> I<FILE>

Configuration file to use. See L<"CONFIGUATION FILE"> for details.

=item B<-p> I<JID password>, B<--password> I<JID password>

The logon password for the JID. You can also set the password via the
configuration file.

=back

=head1 CONFIGURATION FILE

The configutation file is a three document YAML file. The first part contains a
list of multi user chatrooms the bot should join and send messages to. The
syntax is:

    roomname: Nick in this room

The second part is a list of XMPP IDs (Jabber IDs) the bot should send the
notifications to.

    - myself@example.com
    - anotherone@example.com

The last part contains a list of configuration variables (in YAML notation):

=over 8

=item B<ack_notify>

If this option is set to one (1), a notification will be sent out to contacts
indicating that the current service problem has been acknowledged. The default
is 1.

=item B<ack_persistent>

If this option is set to one (1), the comment associated with the
acknowledgement will survive across restarts of the Nagios process. If not, the
comment will be deleted the next time Nagios restarts. The default is 0.

=item B<ack_sticky>

If this option is set to one (1), the acknowledgement will remain until the
host/service return to UP/OK state. Otherwise the acknowledgement will
automatically be removed when the service changes state. The default is 1.

=item B<bot_resource>

XMPP resource of the bot. Defaults to nagibot.pl

=item B<bot_show_idle_after>

Number of minutes before the bot sends an idle presence. The counter is 
resetted each time a nagios or user message arrives. The default is 5.

=item B<bot_status>

XMPP status of the bot. This should be one of 'available', 'away', 'chat',
'dnd' and 'xa'. 

=item B<bot_status_priority>

XMPP status priority of the bot

=item B<bot_status_text>

XMPP status text of the bot.

=item B<nagios_cmd_file>

Nagios(tm) command file. This file accepts the external Nagios(tm) commands
(defaults to /usr/local/nagios/var/rw/nagios.cmd).

=item B<nagios_msg_fifo>

The pipe for the communication with the Nagios(tm) process.

=item B<nagios_status_log_file>

Nagios(tm) status file. This file contains the current state of all Nagios(tm)
objects (defaults to /usr/local/nagios/var/status.dat).

=item B<password>

Logon password of the bot's JID.

=back

=head1 SAMPLE CONFIGURATION

Here is my configuration file:

    ---
    # Rooms to join
    noc@conference.ruhr-uni-bochum.de: NagiBot

    ---
    # JIDs to inform (none in my case)

    ---
    # Configuration
    nagios_status_log_file: /opt/nagios/ver/status.log
    nagios_cmd_file: /opt/nagios/var/rw/nagios.cmd
    nagios_msg_fifo: /opt/nagios/var/rw/nagibot.fifo
    bot_status: available
    bot_status_text: Ich bin ein bot, holt mich hier raus ...
    bot_status_priority: 0
    password: geHa!m
    ---

That's all.

=head1 ACCEPTED COMMANDS

The bot also reacts to some simple commands sent to it. In a MUC (Multi User
Chat) the command has to be preceded by I<@nagibot>.

Here is a list of commands:

=over 8

=item B<help>

The bot reports a list of available commands and a short description of each
command.

=item B<status>

The bot reports the current Nagios(tm) status. It reports how many services are
OK and how many services are in WARNING, CRITICAL or UNKNOWN state.

=item B<details>

The bot reports details about the services that are in a state other than OK.
It reports the hostname, the name of the service and its last state followed by
a new line and the last plugin output.

=item B<info>

The bot reports some information about the system. Currently, only the CPU load
of the Nagios(tm) host system is reported.

=item B<ack> [<service>@]<host> <comment>

The bot will acknowledge the given host or service problem.

=back

=head1 PREREQUISITES

For the bot to work you have to establish a communication channel with the
Nagios(tm) process. Since the bot reads from a pipe you have to create one. You
can do this with the following command:

    mkfifo -m 0660 /path/to/the/pipe

Now you have to instruct the Nagios(tm) process to write to this pipe. An easy
way to do this is to define a "notification command". In my installation I've
done this with the following definition (the file nagios-misccommands contains a
complete example):

    # 'service-notify-by-pipe' command definition
    define command {
    command_name   service-notify-by-pipe
    command_line   /usr/bin/printf "%b" "*$NOTIFICATIONTYPE$* $SERVICEDESC$ \
                       on $HOSTALIAS$ $SERVICESTATE$\n" \
                       >> /opt/nagios/var/rw/nagibot.fifo 2>&1
    }

If you then configure this command to be one of the contacts' notification
command, the bot will be able to get the notifications if Nagios(tm) notifies
that contact. Be sure to add this command only to one contact or you will get
multiple notification messages. And remember: the command_line MUST be on ONE
line!

=cut

# vim: sw=4:si:expandtab:
