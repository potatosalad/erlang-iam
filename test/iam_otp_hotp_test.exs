defmodule IAMOTPHOTPTest do
  use ExUnit.Case, async: true
  use PropCheck

  test "RFC 4226: Appendix D - HOTP Algorithm: Test Values" do
    k = "12345678901234567890"
    assert :iam_otp_hotp.authenticate(k, 0) === "755224"
    assert :iam_otp_hotp.authenticate(k, 1) === "287082"
    assert :iam_otp_hotp.authenticate(k, 2) === "359152"
    assert :iam_otp_hotp.authenticate(k, 3) === "969429"
    assert :iam_otp_hotp.authenticate(k, 4) === "338314"
    assert :iam_otp_hotp.authenticate(k, 5) === "254676"
    assert :iam_otp_hotp.authenticate(k, 6) === "287922"
    assert :iam_otp_hotp.authenticate(k, 7) === "162583"
    assert :iam_otp_hotp.authenticate(k, 8) === "399871"
    assert :iam_otp_hotp.authenticate(k, 9) === "520489"
  end

  property "authenticate and validate" do
    digests = [:sha, :sha256, :sha384, :sha512]
    forall {k, c, hash, digits} in {binary(), non_neg_integer(), oneof(digests), integer(4, 10)} do
      options = %{ hash: hash, digits: digits }
      hotp = :iam_otp_hotp.authenticate(k, c, options)
      :iam_otp_hotp.validate(k, c, hotp, options)
    end
  end

end