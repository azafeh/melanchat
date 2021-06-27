module Shared.Routes (routes) where

import Prelude

import Data.Symbol (class IsSymbol)
import Payload.Client (defaultOpts)
import Payload.Client.Internal.Url (class EncodeUrl)
import Payload.Client.Queryable (class EncodeOptionalQuery, class EncodeUrlWithParams, encodeOptionalQuery, encodeUrlWithParams)
import Payload.Internal.Route (DefaultParentRoute, DefaultRouteSpec)
import Payload.Spec (Spec, Route(Route), Routes)
import Prim.Row (class Nub, class Lacks, class Cons, class Union)
import Prim.RowList (class RowToList, kind RowList, Nil, Cons)
import Prim.Symbol as Symbol
import Record as R
import Shared.Spec (spec)
import Data.String as DS
import Prim.Symbol (class Append)
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce

--this is a hack adapted from payload source code, as I didn't see an builtin/easier way to do it
-- routes :: _
routes = unsafeCoerce 4 -- makeRoutes spec

-- makeRoutes :: forall r routesSpec routesSpecList client. RowToList routesSpec routesSpecList => ToRouteList routesSpecList "" () (Record client) => Spec { routes :: Record routesSpec | r } -> Record client
-- makeRoutes routesSpec = makeRouteList (RLProxy :: _ routesSpecList) (Proxy :: _ "") (Proxy :: _ (Record ()))

-- type ToString payload = payload -> String

-- --REFACTOR: ignore routes that are not GET
-- class ToRouteList (routesSpecList :: RowList) (basePath :: Symbol) (baseParams :: # Type) client | routesSpecList -> client where
--       makeRouteList :: RLProxy routesSpecList -> Proxy basePath -> Proxy (Record baseParams) -> client

-- instance toRouteListNil :: ToRouteList Nil basePath baseParams (Record ()) where
--       makeRouteList _ _ _ = {}

-- instance toRouteListConsRoute :: (
--       IsSymbol parentName,
--       IsSymbol basePath,
--       IsSymbol path,
--       EncodeUrl path childParams,
--       Union parentSpec DefaultParentRoute mergedSpec,
--       Nub mergedSpec parentSpecWithDefaults,
--       TypeEquals (Record parentSpecWithDefaults) { params :: Record parentParams, guards :: parentGuards | childRoutes },
--       Union baseParams parentParams childParams,
--       Cons parentName (Record childClient) remClient client,
--       RowToList childRoutes childRoutesList,
--       Append basePath path childBasePath,
--       ToRouteList childRoutesList childBasePath childParams (Record childClient),
--       Lacks parentName remClient,
--       ToRouteList remRoutes basePath baseParams (Record remClient)
-- ) => ToRouteList (Cons parentName (Routes path (Record parentSpec)) remRoutes) basePath baseParams (Record client) where
--       makeRouteList _ basePath baseParams =
--             R.insert (Proxy :: _ parentName) childRoutes $ makeRouteList (RLProxy :: _ remRoutes) basePath baseParams
--             where childRoutes =
--                         makeRouteList (RLProxy :: _ childRoutesList) (Proxy :: _ childBasePath) (Proxy :: _ (Record childParams))

-- instance toRouteListCons :: (
--       IsSymbol routeName,
--       IsSymbol method,
--       IsSymbol path,
--       Cons routeName (ToString payload) remClient client,
--       Lacks routeName remClient,
--       ToUrlString (Route method path routeSpec) basePath baseParams payload,
--       ToRouteList remRoutes basePath baseParams (Record remClient)
-- ) => ToRouteList (Cons routeName (Route method path routeSpec) remRoutes) basePath baseParams (Record client) where
--       makeRouteList _ _ _ = R.insert (Proxy :: _ routeName) asString rest
--             where rest = makeRouteList (RLProxy :: _ remRoutes) (Proxy :: _ basePath) (Proxy :: _ (Record baseParams))
--                   asString :: ToString payload
--                   asString payload =
--                         makeUrlString (Route :: Route method path routeSpec) (Proxy :: _ basePath) (Proxy :: _ (Record baseParams)) payload

-- class ToUrlString route (basePath :: Symbol) (baseParams :: # Type) payload | route baseParams basePath -> payload where
--       makeUrlString :: route -> Proxy basePath -> Proxy (Record baseParams) -> ToString payload

-- instance queryableGetRoute :: (
--       Lacks "body" route,
--       Union route DefaultRouteSpec mergedRoute,
--       Nub mergedRoute routeWithDefaults,
--       TypeEquals (Record routeWithDefaults) {
--             params :: Record params,
--             query :: query
--             | r
--       },
--       Union baseParams params fullUrlParams,
--       Symbol.Append basePath path fullPath,
--       RowToList fullUrlParams fullParamsList,
--       EncodeUrlWithParams fullPath fullParamsList payload,
--       EncodeOptionalQuery fullPath query payload
-- ) => ToUrlString (Route "GET" path (Record route)) basePath baseParams (Record payload) where
--       makeUrlString _ _ _ payload =
--             let   urlPath = encodeUrlWithParams defaultOpts (Proxy :: _ fullPath) (RLProxy :: _ fullParamsList) payload
--                   urlQuery = encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload
--             in (if DS.null urlPath then "/" else urlPath) <> urlQuery
-- else instance queryablePostRoute :: (
--       Union route DefaultRouteSpec mergedRoute,
--       Nub mergedRoute routeWithDefaults,
--       TypeEquals (Record routeWithDefaults) {
--          params :: Record params,
--          query :: query
--          | r
--       },
--       Union baseParams params fullUrlParams,
--       Symbol.Append basePath path fullPath,
--       RowToList fullUrlParams fullParamsList,
--       EncodeUrlWithParams fullPath fullParamsList payload,
--       EncodeOptionalQuery fullPath query payload
-- ) => ToUrlString (Route "POST" path (Record route)) basePath baseParams (Record payload) where
--       makeUrlString _ _ _ payload =
--             let   urlPath = encodeUrlWithParams defaultOpts (Proxy :: _ fullPath) (RLProxy :: _ fullParamsList) payload
--                   urlQuery = encodeOptionalQuery (Proxy :: _ fullPath) (Proxy :: _ query) payload
--             in urlPath <> urlQuery

