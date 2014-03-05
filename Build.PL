use Module::Build;

Module::Build->new (
  module_name       => 'Nagibot',
  license           => 'perl',
  sign              => 1,
  script_files      => 'nagibot.pl',
  dist_version_from => 'nagibot.pl',
  configure_requires => {
    'Module::Build'                 => '0.38',
  },
  requires          => {
    'AnyEvent'                      => 0,
    'AnyEvent::XMPP'                => '< 0.6',
    'File::Pid'                     => 0,  
    'Nagios::Status::ServiceStatus' => 0,
    'Nagios::Status::HostStatus'    => 0,
    'Sys::CpuLoad'                  => 0,
    'YAML'                          => 0,
    'HTML::Entities'                => 0,
  },
)->create_build_script;
