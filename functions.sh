##
# Encode a password suitable for Teamspeak3's database
#
# $1 -- password to encode
function encode_password_ts3() {
    echo -n "$1" \
      | openssl dgst -binary -sha1 \
      | openssl base64
}
