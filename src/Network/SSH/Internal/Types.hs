module Network.SSH.Internal.Types
( SSH_session(..)
, SSH_key(..)
, SSH_option(..)
, SSH_publickey_hash_type(..)
, SSH_known_hosts_e(..)
) where

import           Foreign.Ptr
import           Foreign.C

newtype SSH_session = SSession { sshSession :: Ptr () }
newtype SSH_key     = SKey     { sshKey     :: Ptr () }

data SSH_option = SSH_OPTIONS_HOST
                | SSH_OPTIONS_PORT
                | SSH_OPTIONS_PORT_STR
                | SSH_OPTIONS_FD
                | SSH_OPTIONS_USER
                | SSH_OPTIONS_SSH_DIR
                | SSH_OPTIONS_IDENTITY
                | SSH_OPTIONS_ADD_IDENTITY
                | SSH_OPTIONS_KNOWNHOSTS
                | SSH_OPTIONS_TIMEOUT
                | SSH_OPTIONS_TIMEOUT_USEC
                | SSH_OPTIONS_SSH1
                | SSH_OPTIONS_SSH2
                | SSH_OPTIONS_LOG_VERBOSITY
                | SSH_OPTIONS_LOG_VERBOSITY_STR
                | SSH_OPTIONS_CIPHERS_C_S
                | SSH_OPTIONS_CIPHERS_S_C
                | SSH_OPTIONS_COMPRESSION_C_S
                | SSH_OPTIONS_COMPRESSION_S_C
                | SSH_OPTIONS_PROXYCOMMAND
                | SSH_OPTIONS_BINDADDR
                | SSH_OPTIONS_STRICTHOSTKEYCHECK
                | SSH_OPTIONS_COMPRESSION
                | SSH_OPTIONS_COMPRESSION_LEVEL
                | SSH_OPTIONS_KEY_EXCHANGE
                | SSH_OPTIONS_HOSTKEYS
                | SSH_OPTIONS_GSSAPI_SERVER_IDENTITY
                | SSH_OPTIONS_GSSAPI_CLIENT_IDENTITY
                | SSH_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS
                | SSH_OPTIONS_HMAC_C_S
                | SSH_OPTIONS_HMAC_S_C
                | SSH_OPTIONS_PASSWORD_AUTH
                | SSH_OPTIONS_PUBKEY_AUTH
                | SSH_OPTIONS_KBDINT_AUTH
                | SSH_OPTIONS_GSSAPI_AUTH
                | SSH_OPTIONS_GLOBAL_KNOWNHOSTS
                | SSH_OPTIONS_NODELAY
                | SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES
                | SSH_OPTIONS_PROCESS_CONFIG
                | SSH_OPTIONS_REKEY_DATA
                | SSH_OPTIONS_REKEY_TIME
                deriving Enum

data SSH_publickey_hash_type = SSH_PUBLICKEY_HASH_SHA1
                             | SSH_PUBLICKEY_HASH_MD5
                             | SSH_PUBLICKEY_HASH_SHA256
                             deriving Enum

data SSH_known_hosts_e = SSH_KNOWN_HOSTS_ERROR
                       | SSH_KNOWN_HOSTS_NOT_FOUND
                       | SSH_KNOWN_HOSTS_UNKNOWN
                       | SSH_KNOWN_HOSTS_OK
                       | SSH_KNOWN_HOSTS_CHANGED
                       | SSH_KNOWN_HOSTS_OTHER

instance Enum SSH_known_hosts_e where
  toEnum (-2) = SSH_KNOWN_HOSTS_ERROR
  toEnum (-1) = SSH_KNOWN_HOSTS_NOT_FOUND
  toEnum ( 0) = SSH_KNOWN_HOSTS_UNKNOWN
  toEnum ( 1) = SSH_KNOWN_HOSTS_OK
  toEnum ( 2) = SSH_KNOWN_HOSTS_CHANGED
  toEnum ( 3) = SSH_KNOWN_HOSTS_OTHER

  fromEnum SSH_KNOWN_HOSTS_ERROR     = -2
  fromEnum SSH_KNOWN_HOSTS_NOT_FOUND = -1
  fromEnum SSH_KNOWN_HOSTS_UNKNOWN   =  0
  fromEnum SSH_KNOWN_HOSTS_OK        =  1
  fromEnum SSH_KNOWN_HOSTS_CHANGED   =  2
  fromEnum SSH_KNOWN_HOSTS_OTHER     =  3
