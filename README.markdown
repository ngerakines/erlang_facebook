erlang\_facebook is a small, lightweight Facebook Platform API client library. It provides several utility functions for request verification and processing as well.

    1> [ApiKey, Secret] = ["aassdd112233", "thisisasecret"].
    ["aassdd112233", "thisisasecret"].
    2> erlang_facebook:custom(ApiKey, Secret, "facebook.users.hasAppPermission", [{"uid", "500025891"}, {"ext_perm", "offline_access"}]).
    [true]

This library is developed and updated as needed. To request a feature or if you found a bug, please contact me through GitHub (account is ngerakines) or email me at [nick+erlang\_facebook@gerakines.net](mailto:nick+erlang_facebook@gerakines.net).

## Supported Facebook API Methods

Facebook API methods map to module functions directly. The list of supported methods can be obtained by looking at the exported functions as per erlang\_facebook:module\_info/0.

To use a method that is not defined the erlang\_facebook:custom/4 function can be used. When passing the function parameters be sure to include a method tuple naming the API method as well as any additional arguments that may be required.

## Dependancies

This module requires MochiWeb, specifically the mochijson2 and mochiweb\_util modules.

## Acknowledgements

Thanks to Brian Fink ([BeerRiot](http://beerriot.com/) rocks!) for the well made [erlang2facebook](http://code.google.com/p/erlang2facebook/) project. It inspired an entire world of Erlang development and inspired this module. Also, thanks to the MochiWeb guys for giving us developers to tools to create great software.
