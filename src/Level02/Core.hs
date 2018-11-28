{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Control.Exception        (bracket_)
import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType (..), Error (..),
                                           RqType (..), mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse s ct bs = responseLBS s [(hContentType, renderContentType ct)] bs

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest t bs = AddRq <$> (mkTopic t) <*> (mkCommentText t2)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict
    t2 = lazyByteStringToStrictText bs

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest :: Text -> Either Error RqType
mkViewRequest t = ViewRq <$> (mkTopic t)

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse e = resp400 PlainText (LBS.fromStrict . encodeUtf8 . pack $ show e)

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest req = case (requestMethod req, pathInfo req) of
                  ("POST",  ["add", t])  -> (mkAddRequest t) <$> (strictRequestBody req)
                  ("GET", [t])           -> return $ mkViewRequest t
                  ("GET", [])            -> return mkListRequest
                  _ -> return $ Left NotFound

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest r = case r of
                    AddRq _ _ -> Right $ resp400 PlainText "AddRq not implemented yet"
                    ViewRq _ -> Right $ resp400 PlainText "ViewRq not implemented yet"
                    ListRq -> Right $ resp400 PlainText "ListRq not implemented yet"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app req resp = do
    req' <- mkRequest req
    case req' of
      Right rqtype -> case handleRequest rqtype of
                        Right r -> resp r
                        Left _ -> resp $ mkErrorResponse NotFound
      Left _ -> resp $ mkErrorResponse NotFound

runApp :: IO ()
runApp = run 8080 app
