%% This is the application resource file (.app file) for the 'base'
%% application.
{application, boot_service,
[{description, "boot_service" },
{vsn, "0.0.1" },
{modules, [boot_service_app,boot_service_sup,
	   boot_service,boot_strap,pod,misc_lib,container]},
{registered,[boot_service]},
{applications, [kernel,stdlib]},
{mod, {boot_service_app,[]}},
{start_phases, []}
]}.
