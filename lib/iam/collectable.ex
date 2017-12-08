defimpl Collectable, for: :iam_bitset do
  def into(original) do
    collector_fun = fn
      bitset, {:cont, elem} -> :iam_bitset.put(bitset, elem)
      bitset, :done -> bitset
      _bitset, :halt -> :ok
    end

    {original, collector_fun}
  end
end

defimpl Collectable, for: :iam_set do
  def into(original) do
    collector_fun = fn
      set, {:cont, elem} -> :iam_set.put(set, elem)
      set, :done -> set
      _set, :halt -> :ok
    end

    {original, collector_fun}
  end
end
