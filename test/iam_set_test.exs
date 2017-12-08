defmodule IAMSetTest do
  use ExUnit.Case, async: true
  use PropCheck

  property "serializability" do
    gen = let {list, separator} <- {list(binary()), byte()} do
      list = delete_byte(list, separator)
      set = :iam_set.new(list)
      string = Enum.join(list, << separator >>)
      {set, string, separator}
    end
    forall {set, string, separator} in gen do
      result_set1 = :iam_set.from_string(string, separator)
      result_string = :iam_set.to_string(set, separator)
      result_set2 = :iam_set.from_string(result_string, separator)
      set === result_set1 and set === result_set2
    end
  end

  test "new/1" do
    result = :iam_set.new(1..5)
    assert :iam_set.equal?(result, Enum.into(1..5, :iam_set.new()))
  end

  test "new/2" do
    result = :iam_set.new(1..5, &(&1 + 2))
    assert :iam_set.equal?(result, Enum.into(3..7, :iam_set.new()))
  end

  test "put/2" do
    result = :iam_set.put(:iam_set.new(), 1)
    assert :iam_set.equal?(result, :iam_set.new([1]))

    result = :iam_set.put(:iam_set.new([1, 3, 4]), 2)
    assert :iam_set.equal?(result, :iam_set.new(1..4))

    result = :iam_set.put(:iam_set.new(5..100), 10)
    assert :iam_set.equal?(result, :iam_set.new(5..100))
  end

  test "union/2" do
    result = :iam_set.union(:iam_set.new([1, 3, 4]), :iam_set.new())
    assert :iam_set.equal?(result, :iam_set.new([1, 3, 4]))

    result = :iam_set.union(:iam_set.new(5..15), :iam_set.new(10..25))
    assert :iam_set.equal?(result, :iam_set.new(5..25))

    result = :iam_set.union(:iam_set.new(1..120), :iam_set.new(1..100))
    assert :iam_set.equal?(result, :iam_set.new(1..120))
  end

  test "intersection/2" do
    result = :iam_set.intersection(:iam_set.new(), :iam_set.new(1..21))
    assert :iam_set.equal?(result, :iam_set.new())

    result = :iam_set.intersection(:iam_set.new(1..21), :iam_set.new(4..24))
    assert :iam_set.equal?(result, :iam_set.new(4..21))

    result = :iam_set.intersection(:iam_set.new(2..100), :iam_set.new(1..120))
    assert :iam_set.equal?(result, :iam_set.new(2..100))
  end

  test "difference/2" do
    result = :iam_set.difference(:iam_set.new(2..20), :iam_set.new())
    assert :iam_set.equal?(result, :iam_set.new(2..20))

    result = :iam_set.difference(:iam_set.new(2..20), :iam_set.new(1..21))
    assert :iam_set.equal?(result, :iam_set.new())

    result = :iam_set.difference(:iam_set.new(1..101), :iam_set.new(2..100))
    assert :iam_set.equal?(result, :iam_set.new([1, 101]))
  end

  test "disjoint?/2" do
    assert :iam_set.disjoint?(:iam_set.new(), :iam_set.new())
    assert :iam_set.disjoint?(:iam_set.new(1..6), :iam_set.new(8..20))
    refute :iam_set.disjoint?(:iam_set.new(1..6), :iam_set.new(5..15))
    refute :iam_set.disjoint?(:iam_set.new(1..120), :iam_set.new(1..6))
  end

  test "subset?/2" do
    assert :iam_set.subset?(:iam_set.new(), :iam_set.new())
    assert :iam_set.subset?(:iam_set.new(1..6), :iam_set.new(1..10))
    assert :iam_set.subset?(:iam_set.new(1..6), :iam_set.new(1..120))
    refute :iam_set.subset?(:iam_set.new(1..120), :iam_set.new(1..6))
  end

  test "equal?/2" do
    assert :iam_set.equal?(:iam_set.new(), :iam_set.new())
    refute :iam_set.equal?(:iam_set.new(1..20), :iam_set.new(2..21))
    assert :iam_set.equal?(:iam_set.new(1..120), :iam_set.new(1..120))
  end

  test "delete/2" do
    result = :iam_set.delete(:iam_set.new(), 1)
    assert :iam_set.equal?(result, :iam_set.new())

    result = :iam_set.delete(:iam_set.new(1..4), 5)
    assert :iam_set.equal?(result, :iam_set.new(1..4))

    result = :iam_set.delete(:iam_set.new(1..4), 1)
    assert :iam_set.equal?(result, :iam_set.new(2..4))

    result = :iam_set.delete(:iam_set.new(1..4), 2)
    assert :iam_set.equal?(result, :iam_set.new([1, 3, 4]))
  end

  test "size/1" do
    assert :iam_set.size(:iam_set.new()) == 0
    assert :iam_set.size(:iam_set.new(5..15)) == 11
    assert :iam_set.size(:iam_set.new(2..100)) == 99
  end

  test "to_list/1" do
    assert :iam_set.to_list(:iam_set.new()) == []

    list = :iam_set.to_list(:iam_set.new(1..20))
    assert Enum.sort(list) == Enum.to_list(1..20)

    list = :iam_set.to_list(:iam_set.new(5..120))
    assert Enum.sort(list) == Enum.to_list(5..120)
  end

  @doc false
  defp delete_byte(list, byte) do
    pattern = :binary.compile_pattern(<< byte >>)
    delete_byte(list, pattern, [])
  end

  @doc false
  defp delete_byte([h | t], pattern, acc) do
    case :erlang.iolist_to_binary(:binary.split(h, pattern, [:global, :trim_all])) do
      <<>> ->
        delete_byte(t, pattern, acc)
      elem ->
        delete_byte(t, pattern, [elem | acc])
    end
  end
  defp delete_byte([], _pattern, acc) do
    :lists.reverse(acc)
  end

end