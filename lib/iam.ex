# import Kernel, except: [
#   raise: 1, raise: 2, throw: 1, throw: 2
# ]

defmodule IAM do

  defmacro raise(class, error) do
    quote do
      :erlang.raise(unquote(class), unquote(error), [hd(elem(:erlang.process_info(:erlang.self(), :current_stacktrace), 1)) | :erlang.get_stacktrace()])
    end
  end

  defmacro raise(class, app, key) do
    quote do
      require IAM
      IAM.raise(unquote(class), :iam_error.new(unquote(app), unquote(key)))
    end
  end

  defmacro raise(class, app, key, data) do
    quote do
      require IAM
      IAM.raise(unquote(class), :iam_error.new(unquote(app), unquote(key), unquote(data)))
    end
  end

  defmacro raise(class, app, key, data, message) do
    quote do
      require IAM
      IAM.raise(unquote(class), :iam_error.new(unquote(app), unquote(key), unquote(data), unquote(message)))
    end
  end

  defmacro throw(app, key) do
    quote do
      :erlang.throw(:iam_error.new(unquote(app), unquote(key)))
    end
  end

  defmacro throw(app, key, data) do
    quote do
      :erlang.throw(:iam_error.new(unquote(app), unquote(key), unquote(data)))
    end
  end

  defmacro throw(app, key, data, message) do
    quote do
      :erlang.throw(:iam_error.new(unquote(app), unquote(key), unquote(data), unquote(message)))
    end
  end

end
