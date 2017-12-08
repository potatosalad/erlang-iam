defmodule IAMCryptoTest do
  use ExUnit.Case, async: true
  use PropCheck
  use Bitwise

  property "constant_time_compare/2" do
    forall subject in binary() do
      short_check =
        if subject == <<>> do
          true
        else
          short = :binary.part(subject, 0, byte_size(subject) - 1)
          not :iam_crypto.constant_time_compare(subject, short) and not :iam_crypto.constant_time_compare(short, subject)
        end
      long = subject <> << 0 >>
      long_check = not :iam_crypto.constant_time_compare(subject, long) and not :iam_crypto.constant_time_compare(long, subject)
      diff_check =
        if subject == <<>> do
          true
        else
          last = :binary.last(subject)
          diff = :binary.part(subject, 0, byte_size(subject) - 1) <> << (last ^^^ 0x01) >>
          not :iam_crypto.constant_time_compare(subject, diff) and not :iam_crypto.constant_time_compare(diff, subject)
        end
      same_check =
        if subject == <<>> do
          true
        else
          :iam_crypto.constant_time_compare(subject, subject)
        end
      short_check and long_check and diff_check and same_check
    end
  end

end