defmodule IAMBaseTest do
  use ExUnit.Case, async: true
  use PropCheck

  property "base16 encode and decode" do
    random_base16 = let size <- nat() do
      :iam_base16.random(size * 2)
    end
    forall {input, random} in {binary(), random_base16} do
      encoded0 = :iam_base16.encode(input)
      encoded1 = :iam_base16.encode(input, case: :upper)
      encoded2 = :iam_base16.encode(input, case: :lower)
      {:ok, decoded0} = :iam_base16.decode(encoded0)
      {:ok, decoded1} = :iam_base16.decode(encoded1)
      {:ok, decoded2} = :iam_base16.decode(encoded2)
      roundtrip = :iam_base16.encode(:iam_base16.decode!(random))
      encoded0 === encoded1 and input === decoded0 and input === decoded1 and input === decoded2 and random === roundtrip
    end
  end

  property "base32 encode and decode" do
    random_base32 = let size <- nat() do
      :iam_base32.random(size * 8)
    end
    forall {input, random} in {binary(), random_base32} do
      encoded0 = :iam_base32.encode(input)
      encoded1 = :iam_base32.encode(input, case: :upper)
      encoded2 = :iam_base32.encode(input, case: :lower)
      encoded3 = :iam_base32.encode(input, case: :lower, padding: false)
      {:ok, decoded0} = :iam_base32.decode(encoded0)
      {:ok, decoded1} = :iam_base32.decode(encoded1)
      {:ok, decoded2} = :iam_base32.decode(encoded2)
      {:ok, decoded3} = :iam_base32.decode(encoded3)
      roundtrip = :iam_base32.encode(:iam_base32.decode!(random))
      encoded0 === encoded1 and input === decoded0 and input === decoded1 and input === decoded2 and input === decoded3 and random === roundtrip
    end
  end

  property "base32hex encode and decode" do
    random_base32hex = let size <- nat() do
      :iam_base32hex.random(size * 8)
    end
    forall {input, random} in {binary(), random_base32hex} do
      encoded0 = :iam_base32hex.encode(input)
      encoded1 = :iam_base32hex.encode(input, case: :upper)
      encoded2 = :iam_base32hex.encode(input, case: :lower)
      encoded3 = :iam_base32hex.encode(input, case: :lower, padding: false)
      {:ok, decoded0} = :iam_base32hex.decode(encoded0)
      {:ok, decoded1} = :iam_base32hex.decode(encoded1)
      {:ok, decoded2} = :iam_base32hex.decode(encoded2)
      {:ok, decoded3} = :iam_base32hex.decode(encoded3)
      roundtrip = :iam_base32hex.encode(:iam_base32hex.decode!(random))
      encoded0 === encoded1 and input === decoded0 and input === decoded1 and input === decoded2 and input === decoded3 and random === roundtrip
    end
  end

  property "base64 encode and decode" do
    random_base64 = let size <- nat() do
      :iam_base64.random(size * 4)
    end
    forall {input, random} in {binary(), random_base64} do
      encoded0 = :iam_base64.encode(input)
      encoded1 = :iam_base64.encode(input, padding: false)
      {:ok, decoded0} = :iam_base64.decode(encoded0)
      {:ok, decoded1} = :iam_base64.decode(encoded1)
      roundtrip = :iam_base64.encode(:iam_base64.decode!(random))
      input === decoded0 and input === decoded1 and random === roundtrip
    end
  end

  property "base64url encode and decode" do
    random_base64url = let size <- nat() do
      :iam_base64url.random(size * 4)
    end
    forall {input, random} in {binary(), random_base64url} do
      encoded0 = :iam_base64url.encode(input)
      encoded1 = :iam_base64url.encode(input, padding: false)
      {:ok, decoded0} = :iam_base64url.decode(encoded0)
      {:ok, decoded1} = :iam_base64url.decode(encoded1)
      roundtrip = :iam_base64url.encode(:iam_base64url.decode!(random))
      input === decoded0 and input === decoded1 and random === roundtrip
    end
  end

end