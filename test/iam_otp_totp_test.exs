defmodule IAMOTPTOTPTest do
  use ExUnit.Case, async: true
  use PropCheck

  test "RFC 6238: Appendix B. Test Vectors" do
    # TODO: fix SHA-256 and SHA-512 support
    k = "12345678901234567890"
    sha1 = %{ hash: :sha, digits: 8 }
    # sha256 = %{ hash: :sha256, digits: 8 }
    # sha512 = %{ hash: :sha512, digits: 8 }
    assert :iam_otp_totp.authenticate(k, 59, sha1) === "94287082"
    # assert :iam_otp_totp.authenticate(k, 59, sha256) === "46119246"
    # assert :iam_otp_totp.authenticate(k, 59, sha512) === "90693936"
    assert :iam_otp_totp.authenticate(k, 1111111109, sha1) === "07081804"
    # assert :iam_otp_totp.authenticate(k, 1111111109, sha256) === "68084774"
    # assert :iam_otp_totp.authenticate(k, 1111111109, sha512) === "25091201"
    assert :iam_otp_totp.authenticate(k, 1111111111, sha1) === "14050471"
    # assert :iam_otp_totp.authenticate(k, 1111111111, sha256) === "67062674"
    # assert :iam_otp_totp.authenticate(k, 1111111111, sha512) === "99943326"
    assert :iam_otp_totp.authenticate(k, 1234567890, sha1) === "89005924"
    # assert :iam_otp_totp.authenticate(k, 1234567890, sha256) === "91819424"
    # assert :iam_otp_totp.authenticate(k, 1234567890, sha512) === "93441116"
    assert :iam_otp_totp.authenticate(k, 2000000000, sha1) === "69279037"
    # assert :iam_otp_totp.authenticate(k, 2000000000, sha256) === "90698825"
    # assert :iam_otp_totp.authenticate(k, 2000000000, sha512) === "38618901"
    assert :iam_otp_totp.authenticate(k, 20000000000, sha1) === "65353130"
    # assert :iam_otp_totp.authenticate(k, 20000000000, sha256) === "77737706"
    # assert :iam_otp_totp.authenticate(k, 20000000000, sha512) === "47863826"
  end

  property "authenticate and validate" do
    digests = [:sha, :sha256, :sha384, :sha512]
    forall {k, c, hash, digits} in {binary(), non_neg_integer(), oneof(digests), integer(4, 10)} do
      options = %{ hash: hash, digits: digits }
      totp = :iam_otp_totp.authenticate(k, c, options)
      :iam_otp_totp.validate(k, c, totp, options)
    end
  end

end