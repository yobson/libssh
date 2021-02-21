{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

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

pattern CEnum e <- (fromIntegral . fromEnum -> e)

ssh_new :: IO SSH_session
ssh_new = SSession <$> [C.exp| void* { ssh_new() } |]
{-# INLINE ssh_new #-}

ssh_free :: SSH_session -> IO ()
ssh_free (SSession p) = [C.exp| void { ssh_free($(void* p)) } |]
{-# INLINE ssh_free #-}

ssh_options_set :: SSH_session -> SSH_option -> Ptr () -> IO ()
ssh_options_set (SSession p) (CEnum o) v = [C.exp| void { ssh_options_set($(void* p), $(int o), $(void* v)) } |]
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
{-# INLINE ssh_key_free #-}

ssh_get_publickey_hash :: SSH_key -> SSH_publickey_hash_type -> IO (CInt, Ptr CUChar, CSize)
ssh_get_publickey_hash (SKey key) (CEnum hash_type) = do
  let hash = nullPtr
  let hlen = 0
  rc <- [C.exp| int { ssh_get_publickey_hash($(void* key), $(int hash_type), (unsigned char**)&$(void* hash), &$(size_t hlen)) } |]
  return (rc, hash, hlen)
{-# INLINE ssh_get_publickey_hash #-}

ssh_session_is_known_server :: SSH_session -> IO SSH_known_hosts_e
ssh_session_is_known_server (SSession p) = toEnum <$> fromIntegral <$> [C.exp| int { ssh_session_is_known_server($(void* p)) }|]
{-# INLINE ssh_session_is_known_server #-}

ssh_print_hash :: SSH_publickey_hash_type -> Ptr CUChar -> CSize -> IO ()
ssh_print_hash (CEnum t) hash hlen = [C.exp| void { ssh_print_hash($(int t), $(unsigned char* hash), $(size_t hlen)) } |]
{-# INLINE ssh_print_hash #-}

ssh_clean_pubkey_hash :: Ptr CUChar -> IO ()
ssh_clean_pubkey_hash hash = [C.exp| void { ssh_clean_pubkey_hash(&$(unsigned char* hash)) } |]
{-# INLINE ssh_clean_pubkey_hash #-}

ssh_string_free_char :: Ptr CChar -> IO ()
ssh_string_free_char hexa = [C.exp| void { ssh_string_free_char($(char* hexa)) } |]
{-# INLINE ssh_string_free_char #-}

ssh_get_hexa :: Ptr CUChar -> CSize -> IO CString
ssh_get_hexa hash size = [C.exp| char* { ssh_get_hexa($(unsigned char* hash), $(size_t size)) } |]
{-# INLINE ssh_get_hexa #-}

ssh_session_update_known_hosts :: SSH_session -> IO CInt
ssh_session_update_known_hosts (SSession s) = [C.exp| int { ssh_session_update_known_hosts($(void* s)) } |]
{-# INLINE ssh_session_update_known_hosts #-}
