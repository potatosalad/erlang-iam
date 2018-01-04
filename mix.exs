defmodule IAM.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :iam,
      version: "0.0.1",
      elixir: "~> 1.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      description: description(),
      name: "iam",
      package: package(),
      source_url: "https://github.com/potatosalad/erlang-iam"
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application() do
    # Specify extra applications you'll use from Erlang/Elixir
    [
      mod: {:iam_app, []},
      extra_applications: [:logger]
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps() do
    [
      {:jose, "~> 1.8"},
      {:ojson, "~> 1.0"},
      {:propcheck, "~> 1.0", only: :test}
    ]
  end

  defp description() do
    """
    Identity and Access Management (IAM)
    """
  end

  defp package() do
    [
      name: :iam,
      files: [
        "CHANGELOG*",
        "include",
        "lib",
        "LICENSE*",
        "mix.exs",
        "priv",
        "README*",
        "rebar.config",
        "src"
      ],
      licenses: ["Mozilla Public License Version 2.0"],
      links: %{
        "GitHub" => "https://github.com/potatosalad/erlang-iam"
      },
      maintainers: ["Andrew Bennett"]
    ]
  end
end
