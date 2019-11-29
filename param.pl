% GLOBAL PARAMETERS OF THE TOG NGAC
%
% all parameters should be defined here
%
:- module(param, [setparam/2,
                  debug/1, statusprt/1,
		  self_test/1, regression_test/1, initialize/1, verbose/1,
		  initialized/1, user_level/1, settable_params/1,
		  self_test_modules/1, regression_test_modules/1,
		  local_pdf_viewer/2, ngac_version/1, ngac_version/2,
                  ngac_current_version_description/1, ngac_name/2,
		  name_string/1, prompt_string/1, msg_failed_command/1,
		  msg_unimplemented_command/1,
		  msg_script_read/1, msg_running_script/1, msg_script_aborted/1,
		  policy_prefix/1, policy_language_version/1, server_version/1,
		  policy_directory_name/1,
		  test_directory_name/1,
		  prettyprint_tab/1,
		  host_os/1, local_pdf_viewer/2, default_policy_file/1,
		  current_policy/1,
                  pqapi_port/1, paapi_port/1, admin_token/1, all_composition/1,
                  server_sleeptime/1, auditable_events/1, audit_selection/1
		 ]).

% Versioning of various things
%
% Past versions ngac_version/2 and current version ngac_version/1:
% When starting a new version create a new ngac_version/1 and
% add a description as a second argument to the preceding version.
%
% When development is actively going on, the current version, given by
% ngac_version/1, is version against which changes are currently being
% actively made and checked-in to the svn repository. It is not fixed.
%
ngac_version('0.1','initial structure setup').
ngac_version('0.1.1', 'initial development' ).
ngac_version('0.2.1', 'initial demo').
ngac_version('0.2.2', 'initial user trial version').
ngac_version('0.2.3', 'cleanup  of trial version' ).
ngac_version('0.3.1', 'initial server version with http interface' ).
ngac_version('0.3.2', 'enhanced server version with separate PQ and PA APIs' ).
ngac_version('0.3.3', 'added \'all\' composition, new options, policy change' ).

ngac_version('0.3.4+++' /* ongoing development of server */ ).
ngac_current_version_description('support for: audit, loadi, readpol, other improvements').

ngac_name('TOG-NGAC','TOG-ngac').

% Flags / Settable params
% enter new params both in dynamic directive and settable_params
%
:- dynamic debug/1, statusprt/1, self_test/1, current_policy/1,
	regression_test/1, initialize/1, initialized/1, verbose/1,
	user_level/1, current_policy/1, pqapi_port/1, paapi_port/1,
        admin_token/1, audit_selection/1.

settable_params([debug,self_test,statusprt,initialize,initialized,regression_test,verbose,
		 user_level,current_policy,pqapi_port,paapi_port,admin_token,audit_selection]).

setparam(Param,Value) :- atom(Param), ground(Value),
    settable_params(SP), memberchk(Param,SP), !,
    P1 =.. [Param,_], retractall(P1),
    P2 =.. [Param,Value], assert(P2).

debug(off). % off/on
statusprt(off). % off/on
self_test(off). % off/on
regression_test(off). % off/on
initialize(on). % off/on
initialized(false).
verbose(off). % off/on
user_level(advanced). % admin/advanced
current_policy('none').
pqapi_port(8001). % default pqapi server port
paapi_port(8001). % default paapi server port, currently same as pqapi
admin_token('admin_token'). % default admin token !?
all_composition(p_uo). % default qualification condition for 'all' composition
server_sleeptime(32767). % sleep main control repeatedly after server start

% Audit events
%   auditable_events is the set of all auditable ngac events
%   audit_selection is the subset of auditable_events currently selected
%     to generate audit records. It can be changed with setparam, but it
%     is the responsibility of the caller of setparam to ensure that
%     only elements of auditable_events are placed in audit_selection.
%
auditable_events([ngac_error,access_grant,access_deny]). % do not remove ngac_error (not maskable)
audit_selection([ngac_error,access_grant,access_deny]).

% Modules providing functional tests
%
self_test_modules([spld]).       % modules that provide self tests
regression_test_modules([spld]). % modules that provide regression tests

% Misc strings
%
name_string('Next Generation Access Control - TOG').
prompt_string('ngac').
msg_failed_command('command failed').
msg_unimplemented_command('Unimplemented command or option. Enter:<command>. help. or quit.').
msg_script_read('script read:').
msg_running_script('running script ...').
msg_script_aborted('script aborted').

policy_prefix('pol_').
policy_language_version('0.3').
server_version('0.3').


% Files and directories
%
policy_directory_name('POLICIES').
test_directory_name('TEST').
default_policy_file('policy').

% Misc values
%
prettyprint_tab(2). % tab indent for pretty printed output

host_os(os_x). % define only one
% host_os(linux). % define only one
% host_os(windows). % define only one

% External utilities
%
local_pdf_viewer(os_x,'"/Applications/Adobe Reader 9/Adobe Reader.app/Contents/MacOS/AdobeReader"').

