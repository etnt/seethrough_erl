{application, seethrough,
 [{description, "seethrough"},
  {vsn, "0.2"},
  {modules, [
    seethrough,
    seethrough_app,
    env,
    dynvar
  ]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib]}]}.
