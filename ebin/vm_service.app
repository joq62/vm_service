%% This is the application resource file (.app file) for the 'base'
%% application.
{application, vm_service,
[{description, "vm_service" },
{vsn, "0.0.1" },
{modules, [vm_service_app,vm_service_sup,
	   vm_service,dns,loader]},
{registered,[vm_service]},
{applications, [kernel,stdlib]},
{mod, {vm_service_app,[]}},
{start_phases, []}
]}.
