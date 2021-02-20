{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.SSH.Internal
( ssh_new
, ssh_free
, ssh_options_set
, ssh_connect
, ssh_disconnect
, ssh_get_server_publickey
, ssh_key_free
, ssh_get_publickey_hash
) where

import qualified Language.C.Inline as C
import           Foreign.Ptr
import           Foreign.C
import           Network.SSH.Internal.Types

C.include "<libssh/libssh.h>"


ssh_new :: IO SSH_session
ssh_new = SSession <$> [C.exp| void* { ssh_new() } |]
{-# INLINE ssh_new #-}

ssh_free :: SSH_session -> IO ()
ssh_free (SSession p) = [C.exp| void { ssh_free($(void* p)) } |]
{-# INLINE ssh_free #-}

ssh_options_set :: SSH_session -> SSH_option -> Ptr () -> IO ()
ssh_options_set (SSession p) o v = let opt = fromIntegral $ fromEnum o
                                   in  [C.exp| void { ssh_options_set($(void* p), $(int opt), $(void* v)) } |]
{-# INLINE ssh_options_set #-}

ssh_connect :: SSH_session -> IO CInt
ssh_connect (SSession p) = [C.exp| int { ssh_connect($(void* p)) } |]
{-# INLINE ssh_connect #-}

ssh_disconnect :: SSH_session -> IO ()
ssh_disconnect (SSession p) = [C.exp| void { ssh_disconnect($(void* p)) } |]
{-# INLINE ssh_disconnect #-}

ssh_get_server_publickey :: SSH_session -> IO (CInt, SSH_key)
ssh_get_server_publickey (SSession p) = do
  let key = nullPtr
  rc <- [C.exp| int { ssh_get_server_publickey($(void* p), (ssh_key*)&$(void* key)) }|]
  return (rc, SKey key)
{-# INLINE ssh_get_server_publickey #-}

ssh_key_free :: SSH_key -> IO ()
ssh_key_free (SKey p) = [C.exp| void {ssh_key_free($(void* p)) } |]

ssh_get_publickey_hash :: SSH_key -> SSH_publickey_hash_type -> IO (CInt, Ptr CUChar, CSize)
ssh_get_publickey_hash (SKey key) t = do
  let hash = nullPtr
  let hlen = 0
  let hash_type = fromIntegral $ fromEnum t
  rc <- [C.exp| int { ssh_get_publickey_hash($(void* key), $(int hash_type), (unsigned char**)&$(void* hash), &$(size_t hlen)) } |]
  return (rc, hash, hlen)
{-# INLINE ssh_get_publickey_hash #-}

ssh_session_is_known_server :: SSH_session -> IO SSH_known_hosts_e
ssh_session_is_known_server (SSession p) = toEnum <$> fromIntegral <$> [C.exp| int { ssh_session_is_known_server($(void* p)) }|]
{-# INLINE ssh_session_is_known_server #-}
