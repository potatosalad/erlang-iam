defmodule IAMBitsetTest do
  use ExUnit.Case, async: true
  use PropCheck

  property "serializability" do
    gen = let legend <- list(binary()) do
      legend = :lists.usort(legend)
      items = random_items_from_legend(legend, 10)
      bitset = :iam_bitset.new(legend, items)
      {bitset, legend}
    end
    forall {bitset, legend} in gen do
      result_string = :iam_bitset.encode(bitset)
      result_bitset = :iam_bitset.decode(:iam_bitset.new(legend), result_string)
      bitset === result_bitset
    end
  end

  test "new/2" do
    result = :iam_bitset.new(1..10, 1..5)
    assert :iam_bitset.equal?(result, Enum.into(1..5, :iam_bitset.new(1..10)))
  end

  test "put/2" do
    result = :iam_bitset.put(:iam_bitset.new(1..10), 1)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, [1]))

    result = :iam_bitset.put(:iam_bitset.new(1..10, [1, 3, 4]), 2)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, 1..4))

    result = :iam_bitset.put(:iam_bitset.new(1..100, 5..100), 10)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..100, 5..100))
  end

  test "put_enum/2" do
    result = :iam_bitset.put_enum(:iam_bitset.new(1..10), [1])
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, [1]))

    result = :iam_bitset.put_enum(:iam_bitset.new(1..10, [1, 3]), [4, 2])
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, 1..4))

    result = :iam_bitset.put_enum(:iam_bitset.new(1..100, 10..15), 5..100)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..100, 5..100))
  end

  test "union/2" do
    result = :iam_bitset.union(:iam_bitset.new(1..10, [1, 3, 4]), :iam_bitset.new(1..10))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, [1, 3, 4]))

    result = :iam_bitset.union(:iam_bitset.new(1..25, 5..15), :iam_bitset.new(1..25, 10..25))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..25, 5..25))

    result = :iam_bitset.union(:iam_bitset.new(1..200, 1..120), :iam_bitset.new(1..200, 1..100))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..200, 1..120))
  end

  test "intersection/2" do
    result = :iam_bitset.intersection(:iam_bitset.new(1..30), :iam_bitset.new(1..30, 1..21))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..30))

    result = :iam_bitset.intersection(:iam_bitset.new(1..30, 1..21), :iam_bitset.new(1..30, 4..24))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..30, 4..21))

    result = :iam_bitset.intersection(:iam_bitset.new(1..200, 2..100), :iam_bitset.new(1..200, 1..120))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..200, 2..100))
  end

  test "difference/2" do
    result = :iam_bitset.difference(:iam_bitset.new(1..30, 2..20), :iam_bitset.new(1..30))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..30, 2..20))

    result = :iam_bitset.difference(:iam_bitset.new(1..30, 2..20), :iam_bitset.new(1..30, 1..21))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..30))

    result = :iam_bitset.difference(:iam_bitset.new(1..200, 1..101), :iam_bitset.new(1..200, 2..100))
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..200, [1, 101]))
  end

  test "disjoint?/2" do
    assert :iam_bitset.disjoint?(:iam_bitset.new(), :iam_bitset.new())
    assert :iam_bitset.disjoint?(:iam_bitset.new(1..20, 1..6), :iam_bitset.new(1..20, 8..20))
    refute :iam_bitset.disjoint?(:iam_bitset.new(1..20, 1..6), :iam_bitset.new(1..20, 5..15))
    refute :iam_bitset.disjoint?(:iam_bitset.new(1..200, 1..120), :iam_bitset.new(1..200, 1..6))
  end

  test "subset?/2" do
    assert :iam_bitset.subset?(:iam_bitset.new(), :iam_bitset.new())
    assert :iam_bitset.subset?(:iam_bitset.new(1..20, 1..6), :iam_bitset.new(1..20, 1..10))
    assert :iam_bitset.subset?(:iam_bitset.new(1..200, 1..6), :iam_bitset.new(1..200, 1..120))
    refute :iam_bitset.subset?(:iam_bitset.new(1..200, 1..120), :iam_bitset.new(1..200, 1..6))
  end

  test "equal?/2" do
    assert :iam_bitset.equal?(:iam_bitset.new(), :iam_bitset.new())
    assert :iam_bitset.equal?(:iam_bitset.new(1..10), :iam_bitset.new(1..10))
    refute :iam_bitset.equal?(:iam_bitset.new(1..10), :iam_bitset.new(1..5))
    refute :iam_bitset.equal?(:iam_bitset.new(1..30, 1..20), :iam_bitset.new(1..30, 2..21))
    assert :iam_bitset.equal?(:iam_bitset.new(1..200, 1..120), :iam_bitset.new(1..200, 1..120))
  end

  test "delete/2" do
    result = :iam_bitset.delete(:iam_bitset.new(1..10), 1)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10))

    result = :iam_bitset.delete(:iam_bitset.new(1..10, 1..4), 5)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, 1..4))

    result = :iam_bitset.delete(:iam_bitset.new(1..10, 1..4), 1)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, 2..4))

    result = :iam_bitset.delete(:iam_bitset.new(1..10, 1..4), 2)
    assert :iam_bitset.equal?(result, :iam_bitset.new(1..10, [1, 3, 4]))
  end

  test "size/1" do
    assert :iam_bitset.size(:iam_bitset.new()) == 0
    assert :iam_bitset.size(:iam_bitset.new(1..10)) == 0
    assert :iam_bitset.size(:iam_bitset.new(1..20, 5..15)) == 11
    assert :iam_bitset.size(:iam_bitset.new(1..100, 2..100)) == 99
  end

  test "to_list/1" do
    assert :iam_bitset.to_list(:iam_bitset.new()) == []
    assert :iam_bitset.to_list(:iam_bitset.new(1..10)) == []

    list = :iam_bitset.to_list(:iam_bitset.new(1..20, 1..20))
    assert Enum.sort(list) == Enum.to_list(1..20)

    list = :iam_bitset.to_list(:iam_bitset.new(1..200, 5..120))
    assert Enum.sort(list) == Enum.to_list(5..120)
  end

  @doc false
  defp random_items_from_legend(legend, n) when is_list(legend) and is_integer(n) and n >= 0 do
    random_items_from_legend(legend, length(legend), n, [])
  end

  @doc false
  defp random_items_from_legend(_legend, _len, 0, acc) do
    acc
  end
  defp random_items_from_legend(_legend, 0, _n, acc) do
    acc
  end
  defp random_items_from_legend(legend, len, n, acc) do
    item = :lists.nth(:rand.uniform(len), legend)
    random_items_from_legend(legend, len, n - 1, [item | acc])
  end

end