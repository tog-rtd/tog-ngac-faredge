:- module(policies, [policy/3]).
% Example policies used for built-in self-test
%   All policies defined in this module will be automatically included
%   in the initialization of the policy database in spld.
%   'Policy (a)', 'Policy (b)', 'Signals Access Policy' and 'Vehicle Ownership Policy'
%   are assumed to be available to the self tests for spld.
%   DO NOT REMOVE THEM!
%   It is OK to add others.
%
% policy(PolicyName, PolicyRoot, PolicyElements)

:- dynamic policy/3.

%
% Do not modify the following 4 policies

policy('Policy (a)','Project Access', [
	user('u1'),
	user('u2'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	policy_class('Project Access'),
	connector('PM'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Project Access'),
	assign('Projects','Project Access'),
	assign('Gr2-Secret','Project Access'),
        assign('Project Access','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w],'Gr2-Secret'),
	associate('Division',[r],'Projects')
	]).

policy('Policy (b)','File Management', [
	user('u1'),
	user('u2'),
	user_attribute('Alice'),
	user_attribute('Bob'),
	user_attribute('Users'),
	object('o2'),
	object('o3'),
	object('o4'),
	object_attribute('Proposals'),
	object_attribute('Reports'),
	object_attribute('Bob Home'),
	policy_class('File Management'),
	connector('PM'),
	assign('u1','Alice'),
	assign('u2','Bob'),
	assign('Alice','Users'),
	assign('Bob','Users'),
	assign('o2','Proposals'),
	assign('o3','Reports'),
	assign('o4','Reports'),
	assign('Proposals','Bob Home'),
	assign('Reports','Bob Home'),
	assign('Users','File Management'),
	assign('Bob Home','File Management'),
        assign('File Management','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Bob',[r,w],'Bob Home'),
	associate('Alice',[r,w],'o2')
	]).

policy('Signals Access Policy','Signals Access', [
        user('Sebastian'),
        user('Ana'),
        user('OEM employee 1'),

        user_attribute('Vehicle Owners'),
        user_attribute('Vehicle OEM'),

        object('VIN-1001 Shift Signals'),
        object('VIN-1001 Window Signals'),
        object('VIN-1001 Door Signals'),
        object('VIN-1001 Trip Signals'),

        object('VIN-1002 Shift Signals'),
        object('VIN-1002 Window Signals'),
        object('VIN-1002 Door Signals'),
        object('VIN-1002 Trip Signals'),

        object('VIN-2001 Shift Signals'),
        object('VIN-2001 Window Signals'),
        object('VIN-2001 Door Signals'),
        object('VIN-2001 Trip Signals'),

        object('VIN-3001 Shift Signals'),
        object('VIN-3001 Window Signals'),
        object('VIN-3001 Door Signals'),
        object('VIN-3001 Trip Signals'),

        object_attribute('Trip Signals'),
        object_attribute('Window Signals'),
        object_attribute('Door Signals'),
        object_attribute('Shift Signals'),

        object_attribute('Owner Accessible Signals'),
        object_attribute('OEM Accessible Signals'),

        policy_class('Signals Access'),

        connector('PM'),

        assign('Sebastian', 'Vehicle Owners'),
        assign('Ana', 'Vehicle Owners'),

        assign('OEM employee 1', 'Vehicle OEM'),

        assign('Vehicle OEM', 'Vehicle Owners'),

        assign('VIN-1001 Shift Signals', 'Shift Signals'),
        assign('VIN-1001 Window Signals', 'Window Signals'),
        assign('VIN-1001 Door Signals', 'Door Signals'),
        assign('VIN-1001 Trip Signals', 'Trip Signals'),

        assign('VIN-1002 Shift Signals', 'Shift Signals'),
        assign('VIN-1002 Window Signals', 'Window Signals'),
        assign('VIN-1002 Door Signals', 'Door Signals'),
        assign('VIN-1002 Trip Signals', 'Trip Signals'),

        assign('VIN-2001 Shift Signals', 'Shift Signals'),
        assign('VIN-2001 Window Signals', 'Window Signals'),
        assign('VIN-2001 Door Signals', 'Door Signals'),
        assign('VIN-2001 Trip Signals', 'Trip Signale'),

        assign('VIN-3001 Shift Signals', 'Shift Signals'),
        assign('VIN-3001 Window Signals', 'Window Signals'),
        assign('VIN-3001 Door Signals', 'Door Signals'),
        assign('VIN-3001 Trip Signals', 'Trip Signals'),

        assign('Trip Signals', 'OEM Accessible Signals'),

        assign('Window Signals', 'Owner Accessible Signals'),
        assign('Door Signals', 'Owner Accessible Signals'),
        assign('Shift Signals', 'Owner Accessible Signals'),

        assign('Owner Accessible Signals', 'OEM Accessible Signals'),

        assign('Vehicle Owners', 'Signals Access'),
        assign('OEM Accessible Signals', 'Signals Access'),
        assign('Owner Accessible Signals', 'Signals Access'),

        assign('Signals Access','PM'),

        associate('Vehicle Owners', [r], 'Owner Accessible Signals'),
        associate('Vehicle OEM', [r,w], 'OEM Accessible Signals')
]).

policy('Vehicle Ownership Policy','Vehicle Ownership', [
	user('Sebastian'),
        user('Ana'),

	user_attribute('Scholze Family'),
        user_attribute('Correia Family'),

        user_attribute('Owners'),

	object('VIN-1001 Shift Signals'),
	object('VIN-1001 Window Signals'),
	object('VIN-1001 Door Signals'),
	object('VIN-1001 Trip Signals'),

	object('VIN-1002 Shift Signals'),
	object('VIN-1002 Window Signals'),
	object('VIN-1002 Door Signals'),
	object('VIN-1002 Trip Signals'),

	object('VIN-2001 Shift Signals'),
	object('VIN-2001 Window Signals'),
	object('VIN-2001 Door Signals'),
	object('VIN-2001 Trip Signals'),

	object('VIN-3001 Shift Signals'),
	object('VIN-3001 Window Signals'),
	object('VIN-3001 Door Signals'),
	object('VIN-3001 Trip Signals'),

	object_attribute('Vehicle VIN-1001'),
	object_attribute('Vehicle VIN-1002'),
	object_attribute('Vehicle VIN-2001'),
	object_attribute('Vehicle VIN-3001'),

	object_attribute('Scholze Family Vehicles'),
	object_attribute('Correia Family Vehicles'),

	object_attribute('Vehicles'),

	policy_class('Vehicle Ownership'),

	connector('PM'),

	assign('Sebastian', 'Scholze Family'),
	assign('Ana', 'Correia Family'),

	assign('Scholze Family', 'Owners'),
	assign('Correia Family', 'Owners'),

	assign('VIN-1001 Shift Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Window Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Door Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Trip Signals', 'Vehicle VIN-1001'),

	assign('VIN-1002 Shift Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Window Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Door Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Trip Signals', 'Vehicle VIN-1002'),

	assign('VIN-2001 Shift Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Window Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Door Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Trip Signals', 'Vehicle VIN-2001'),

	assign('VIN-3001 Shift Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Window Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Door Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Trip Signals', 'Vehicle VIN-3001'),

	assign('Vehicle VIN-1001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-1002', 'Correia Family Vehicles'),
	assign('Vehicle VIN-2001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-3001', 'Correia Family Vehicles'),

	assign('Scholze Family Vehicles', 'Vehicles'),
	assign('Correia Family Vehicles', 'Vehicles'),

	assign('Owners', 'Vehicle Ownership'),
	assign('Vehicles', 'Vehicle Ownership'),

	assign('Vehicle Ownership', 'PM'),

	associate('Scholze Family',[o,r],'Scholze Family Vehicles'),
	associate('Correia Family',[o,r],'Correia Family Vehicles')
	]).

% DO NOT DELETE OR MODIFY THE PRECEDING 4 POLICIES
%
% Others may be added below
%

policy('Policy (aa)','Project Access 1', [
	user('u1'),
	user('u2'),
        user('u3'),
        user('u5'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	policy_class('Project Access 1'),
	connector('PM'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Project Access 1'),
	assign('Projects','Project Access 1'),
	assign('Gr2-Secret','Project Access 1'),
        assign('Project Access 1','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w],'Gr2-Secret'),
	associate('Division',[r],'Projects')
	]).

policy('Policy (bb)','File Management 1', [
	user('u1'),
	user('u2'),
        user('u4'),
        user('u5'),
	user_attribute('Alice'),
	user_attribute('Bob'),
	user_attribute('Users'),
	object('o2'),
	object('o3'),
	object('o4'),
	object_attribute('Proposals'),
	object_attribute('Reports'),
	object_attribute('Bob Home'),
	policy_class('File Management 1'),
	connector('PM'),
	assign('u1','Alice'),
	assign('u2','Bob'),
	assign('Alice','Users'),
	assign('Bob','Users'),
	assign('o2','Proposals'),
	assign('o3','Reports'),
	assign('o4','Reports'),
	assign('Proposals','Bob Home'),
	assign('Reports','Bob Home'),
	assign('Users','File Management 1'),
	assign('Bob Home','File Management 1'),
        assign('File Management 1','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Bob',[r,w],'Bob Home'),
	associate('Alice',[r,w],'o2')
	]).

