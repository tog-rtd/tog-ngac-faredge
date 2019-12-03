er_package(er_test1, [
    er(
        ev_pat(user(any),policy_class(any),operation(adnull),object(any)),
        [addm([]),deletem([])]
    ),
    er(
        ev_pat(user(any),policy_class(any),operation(addone),object(any)),
        [add('Policy (a)',user(u4))]
    ),
    er(
        ev_pat(user(any),policy_class(any),operation(deletem1),object(any)),
        [
         deletem('Policy (a)',[
                     user(u4),
                     assign(u2,'Group2'),
                     user(u2)
              ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(addm1),object(any)),
        [
         addm('Policy (a)',[
                  user(u2),
                  assign(u2,'Group2')
                 ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(deletem2),object(any)),
        [
         deletem('Policy (a)',[
                     assign(u2,'Group2'),
                     user(u2)
              ])
        ]),
    er(
        ev_pat(user(any),policy_class(any),operation(addm2),object(any)),
        [
         addm('Policy (a)',[
                  user(u2),
                  assign(u2,'Group2')
                 ])
        ])
]).


%curl -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u4)" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u4)" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=assign(u2,'Group2')" --data-urlencode "token=admin_token"
%curl -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=Policy (a)" --data-urlencode "policy_element=user(u2)" --data-urlencode "token=admin_token"
