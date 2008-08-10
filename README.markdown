
erlang\facebook is a small, lightweight Facebook Platform API client library. It uses a gen\_server behavior to create, dispatch and process Facebook Platform API requests.

    1> erlang_facebook:start().
    ok
    2> [ApiKey, Secret] = ["aassdd112233", "thisisasecret"].
    ["aassdd112233", "thisisasecret"].
    3> erlang_facebook:update(ApiKey, Secret).
    ok
    4> erlang_facebook:call(application_getpublicinfo, [
    4>     {"application_canvas_name", "iplaywow"}
    4> ]).
    {obj,[{"app_id", 2359644980},
          {"api_key", ...},
          {"canvas_name",<<"iplaywow">>},
          {"display_name",<<"I Play WoW">>},
    5> erlang_facebook:call(custom, [
    5>     {"method", "facebook.admin.getAllocation"},
    5>     {"integration_point_name", "requests_per_day"}
    5> ]).
    31

This library is developed and updated as needed. To request a feature or if you found a bug, please contact me through GitHub (account is ngerakines) or email me at [nick+erlang\_facebook@gerakines.net](mailto:nick+erlang_facebook@gerakines.net).

## Supported Facebook API Methods

Facebook API methods map to module functions directly. The list of supported methods can be obtained by looking at the exported functions as per erlang\_facebook:module\_info/0.

To use a method that is not defined the erlang\_facebook:custom/2 function can be used. When passing the function parameters be sure to include a method tuple naming the API method as well as any additional arguments that may be required.

## Dependancies

By default, this module sets the 'format' parameter of outgoing API requests to 'JSON' and uses the parse\_json/1 callback function to process the responses. To parse json requests this module uses the [rfc4627](http://hg.opensource.lshift.net/erlang-rfc4627/) module provided by LShift inc. 

## Acknowledgements

Thanks to Brian Fink ([BeerRiot](http://beerriot.com/) rocks!) for the well made [erlang2facebook](http://code.google.com/p/erlang2facebook/) project. It inspired an entire world of Erlang development and inspired this module.
