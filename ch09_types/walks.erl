-module(walks).
-export([plan_route/2]).

%% illustrate basic type annotations
%% type specification
-spec plan_route(From:: point(), To:: point()) -> route().
%% type declaration
-type direction() :: north | south | east | west.
-type point() :: {integer(), integer()}.
-type route() :: [{go, direction(), integer()}].

-type angle() :: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.
-type position() :: {latitude | longitude, angle()}.
-spec plan_route1(From::position(), To::position()) -> route().