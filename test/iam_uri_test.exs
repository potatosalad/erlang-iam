defmodule IAMURITest do
  use ExUnit.Case, async: true

  # doctest URI

  test "encode/1,2" do
    assert :iam_uri.encode("4_test.is-s~") == "4_test.is-s~"
    assert :iam_uri.encode("\r\n&<%>\" ゆ", &:iam_uri.char_unreserved?/1) == "%0D%0A%26%3C%25%3E%22%20%E3%82%86"
  end

  test "encode_www_form/1" do
    assert :iam_uri.encode_www_form("4test ~1.x") == "4test+~1.x"
    assert :iam_uri.encode_www_form("poll:146%") == "poll%3A146%25"
    assert :iam_uri.encode_www_form("/\n+/ゆ") == "%2F%0A%2B%2F%E3%82%86"
  end

  test "encode_query/1" do
    assert :iam_uri.encode_query([{:foo, :bar}, {:baz, :quux}]) == "foo=bar&baz=quux"
    assert :iam_uri.encode_query([{"foo", "bar"}, {"baz", "quux"}]) == "foo=bar&baz=quux"
    assert :iam_uri.encode_query([{"foo z", :bar}]) == "foo+z=bar"

    assert_raise ArgumentError, fn ->
      :iam_uri.encode_query([{"foo", 'bar'}])
    end

    assert_raise ArgumentError, fn ->
      :iam_uri.encode_query([{'foo', "bar"}])
    end
  end

  test "decode_query/1,2" do
    assert :iam_uri.decode_query("", %{}) == %{}

    assert :iam_uri.decode_query("safe=off", %{"cookie" => "foo"}) ==
             %{"safe" => "off", "cookie" => "foo"}

    assert :iam_uri.decode_query("q=search%20query&cookie=ab%26cd&block+buster=") ==
             %{"block buster" => "", "cookie" => "ab&cd", "q" => "search query"}

    assert :iam_uri.decode_query("something=weird%3Dhappening") == %{"something" => "weird=happening"}

    assert :iam_uri.decode_query("garbage") == %{"garbage" => nil}
    assert :iam_uri.decode_query("=value") == %{"" => "value"}
    assert :iam_uri.decode_query("something=weird=happening") == %{"something" => "weird=happening"}
  end

  test "decode/1" do
    assert :iam_uri.decode("%0D%0A%26%3C%25%3E%22%20%E3%82%86") == "\r\n&<%>\" ゆ"
    assert :iam_uri.decode("%2f%41%4a%55") == "/AJU"
    assert :iam_uri.decode("4_t+st.is-s~") == "4_t+st.is-s~"

    assert_raise ArgumentError, ~R/malformed URI/, fn ->
      :iam_uri.decode("% invalid")
    end

    assert_raise ArgumentError, ~R/malformed URI/, fn ->
      :iam_uri.decode("invalid%")
    end
  end

  test "decode_www_form/1" do
    assert :iam_uri.decode_www_form("%3Eval+ue%2B") == ">val ue+"
    assert :iam_uri.decode_www_form("%E3%82%86+") == "ゆ "

    assert_raise ArgumentError, fn ->
      :iam_uri.decode_www_form("%ZZ")
    end
  end

  # describe "parse/1" do
  #   test "returns the given URI if a %URI{} struct is given" do
  #     assert :iam_uri.parse(uri = %URI{scheme: "http", host: "foo.com"}) == uri
  #   end

  #   test "works with HTTP scheme" do
  #     expected_uri = %URI{
  #       scheme: "http",
  #       host: "foo.com",
  #       path: "/path/to/something",
  #       query: "foo=bar&bar=foo",
  #       fragment: "fragment",
  #       port: 80,
  #       authority: "foo.com",
  #       userinfo: nil
  #     }

  #     assert :iam_uri.parse("http://foo.com/path/to/something?foo=bar&bar=foo#fragment") ==
  #              expected_uri
  #   end

  #   test "works with HTTPS scheme" do
  #     expected_uri = %URI{
  #       scheme: "https",
  #       host: "foo.com",
  #       authority: "foo.com",
  #       query: nil,
  #       fragment: nil,
  #       port: 443,
  #       path: nil,
  #       userinfo: nil
  #     }

  #     assert :iam_uri.parse("https://foo.com") == expected_uri
  #   end

  #   test "works with \"file\" scheme" do
  #     expected_uri = %URI{
  #       scheme: "file",
  #       host: nil,
  #       path: "/foo/bar/baz",
  #       userinfo: nil,
  #       query: nil,
  #       fragment: nil,
  #       port: nil,
  #       authority: nil
  #     }

  #     assert :iam_uri.parse("file:///foo/bar/baz") == expected_uri
  #   end

  #   test "works with FTP scheme" do
  #     expected_uri = %URI{
  #       scheme: "ftp",
  #       host: "private.ftp-server.example.com",
  #       userinfo: "user001:password",
  #       authority: "user001:password@private.ftp-server.example.com",
  #       path: "/my_directory/my_file.txt",
  #       query: nil,
  #       fragment: nil,
  #       port: 21
  #     }

  #     ftp = "ftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
  #     assert :iam_uri.parse(ftp) == expected_uri
  #   end

  #   test "works with SFTP scheme" do
  #     expected_uri = %URI{
  #       scheme: "sftp",
  #       host: "private.ftp-server.example.com",
  #       userinfo: "user001:password",
  #       authority: "user001:password@private.ftp-server.example.com",
  #       path: "/my_directory/my_file.txt",
  #       query: nil,
  #       fragment: nil,
  #       port: 22
  #     }

  #     sftp = "sftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
  #     assert :iam_uri.parse(sftp) == expected_uri
  #   end

  #   test "works with TFTP scheme" do
  #     expected_uri = %URI{
  #       scheme: "tftp",
  #       host: "private.ftp-server.example.com",
  #       userinfo: "user001:password",
  #       authority: "user001:password@private.ftp-server.example.com",
  #       path: "/my_directory/my_file.txt",
  #       query: nil,
  #       fragment: nil,
  #       port: 69
  #     }

  #     tftp = "tftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
  #     assert :iam_uri.parse(tftp) == expected_uri
  #   end

  #   test "works with LDAP scheme" do
  #     expected_uri = %URI{
  #       scheme: "ldap",
  #       host: nil,
  #       authority: nil,
  #       userinfo: nil,
  #       path: "/dc=example,dc=com",
  #       query: "?sub?(givenName=John)",
  #       fragment: nil,
  #       port: 389
  #     }

  #     assert :iam_uri.parse("ldap:///dc=example,dc=com??sub?(givenName=John)") == expected_uri

  #     expected_uri = %URI{
  #       scheme: "ldap",
  #       host: "ldap.example.com",
  #       authority: "ldap.example.com",
  #       userinfo: nil,
  #       path: "/cn=John%20Doe,dc=foo,dc=com",
  #       fragment: nil,
  #       port: 389,
  #       query: nil
  #     }

  #     assert :iam_uri.parse("ldap://ldap.example.com/cn=John%20Doe,dc=foo,dc=com") == expected_uri
  #   end

  #   test "splits authority" do
  #     expected_uri = %URI{
  #       scheme: "http",
  #       host: "foo.com",
  #       path: nil,
  #       query: nil,
  #       fragment: nil,
  #       port: 4444,
  #       authority: "foo:bar@foo.com:4444",
  #       userinfo: "foo:bar"
  #     }

  #     assert :iam_uri.parse("http://foo:bar@foo.com:4444") == expected_uri

  #     expected_uri = %URI{
  #       scheme: "https",
  #       host: "foo.com",
  #       path: nil,
  #       query: nil,
  #       fragment: nil,
  #       port: 443,
  #       authority: "foo:bar@foo.com",
  #       userinfo: "foo:bar"
  #     }

  #     assert :iam_uri.parse("https://foo:bar@foo.com") == expected_uri

  #     expected_uri = %URI{
  #       scheme: "http",
  #       host: "foo.com",
  #       path: nil,
  #       query: nil,
  #       fragment: nil,
  #       port: 4444,
  #       authority: "foo.com:4444",
  #       userinfo: nil
  #     }

  #     assert :iam_uri.parse("http://foo.com:4444") == expected_uri
  #   end

  #   test "can parse bad URIs" do
  #     assert :iam_uri.parse("")
  #     assert :iam_uri.parse("https:??@?F?@#>F//23/")

  #     assert :iam_uri.parse(":https").path == ":https"
  #     assert :iam_uri.parse("https").path == "https"
  #     assert :iam_uri.parse("ht\0tps://foo.com").path == "ht\0tps://foo.com"
  #   end

  #   test "can parse IPv6 addresses" do
  #     addresses = [
  #       # undefined
  #       "::",
  #       # loopback
  #       "::1",
  #       # unicast
  #       "1080::8:800:200C:417A",
  #       # multicast
  #       "FF01::101",
  #       # abbreviated
  #       "2607:f3f0:2:0:216:3cff:fef0:174a",
  #       # mixed hex case
  #       "2607:f3F0:2:0:216:3cFf:Fef0:174A",
  #       # complete
  #       "2051:0db8:2d5a:3521:8313:ffad:1242:8e2e",
  #       # embedded IPv4
  #       "::00:192.168.10.184"
  #     ]

  #     Enum.each(addresses, fn addr ->
  #       simple_uri = :iam_uri.parse("http://[#{addr}]/")
  #       assert simple_uri.authority == "[#{addr}]"
  #       assert simple_uri.host == addr

  #       userinfo_uri = :iam_uri.parse("http://user:pass@[#{addr}]/")
  #       assert userinfo_uri.authority == "user:pass@[#{addr}]"
  #       assert userinfo_uri.host == addr
  #       assert userinfo_uri.userinfo == "user:pass"

  #       port_uri = :iam_uri.parse("http://[#{addr}]:2222/")
  #       assert port_uri.authority == "[#{addr}]:2222"
  #       assert port_uri.host == addr
  #       assert port_uri.port == 2222

  #       userinfo_port_uri = :iam_uri.parse("http://user:pass@[#{addr}]:2222/")
  #       assert userinfo_port_uri.authority == "user:pass@[#{addr}]:2222"
  #       assert userinfo_port_uri.host == addr
  #       assert userinfo_port_uri.userinfo == "user:pass"
  #       assert userinfo_port_uri.port == 2222
  #     end)
  #   end

  #   test "downcases the scheme" do
  #     assert :iam_uri.parse("hTtP://google.com").scheme == "http"
  #   end

  #   test "preserves empty fragments" do
  #     assert :iam_uri.parse("http://example.com#").fragment == ""
  #     assert :iam_uri.parse("http://example.com/#").fragment == ""
  #     assert :iam_uri.parse("http://example.com/test#").fragment == ""
  #   end
  # end

  # test "default_port/1,2" do
  #   assert :iam_uri.default_port("http") == 80

  #   try do
  #     :iam_uri.default_port("http", 8000)
  #     assert :iam_uri.default_port("http") == 8000
  #   after
  #     :iam_uri.default_port("http", 80)
  #   end

  #   assert :iam_uri.default_port("unknown") == nil
  #   :iam_uri.default_port("unknown", 13)
  #   assert :iam_uri.default_port("unknown") == 13
  # end

  # test "to_string/1 and Kernel.to_string/1" do
  #   assert to_string(:iam_uri.parse("http://google.com")) == "http://google.com"
  #   assert to_string(:iam_uri.parse("http://google.com:443")) == "http://google.com:443"
  #   assert to_string(:iam_uri.parse("https://google.com:443")) == "https://google.com"
  #   assert to_string(:iam_uri.parse("http://lol:wut@google.com")) == "http://lol:wut@google.com"
  #   assert to_string(:iam_uri.parse("http://google.com/elixir")) == "http://google.com/elixir"
  #   assert to_string(:iam_uri.parse("http://google.com?q=lol")) == "http://google.com?q=lol"
  #   assert to_string(:iam_uri.parse("http://google.com?q=lol#omg")) == "http://google.com?q=lol#omg"
  #   assert to_string(:iam_uri.parse("//google.com/elixir")) == "//google.com/elixir"
  #   assert to_string(:iam_uri.parse("//google.com:8080/elixir")) == "//google.com:8080/elixir"
  #   assert to_string(:iam_uri.parse("//user:password@google.com/")) == "//user:password@google.com/"
  #   assert to_string(:iam_uri.parse("http://[2001:db8::]:8080")) == "http://[2001:db8::]:8080"
  #   assert to_string(:iam_uri.parse("http://[2001:db8::]")) == "http://[2001:db8::]"

  #   assert :iam_uri.to_string(:iam_uri.parse("http://google.com")) == "http://google.com"

  #   assert :iam_uri.to_string(:iam_uri.parse("//user:password@google.com/")) ==
  #            "//user:password@google.com/"
  # end

  # test "merge/2" do
  #   assert_raise ArgumentError, "you must merge onto an absolute URI", fn ->
  #     :iam_uri.merge("/relative", "")
  #   end

  #   assert :iam_uri.merge("http://google.com/foo", "http://example.com/baz")
  #          |> to_string == "http://example.com/baz"

  #   assert :iam_uri.merge("http://google.com/foo", "http://example.com/.././bar/../../baz")
  #          |> to_string == "http://example.com/baz"

  #   assert :iam_uri.merge("http://google.com/foo", "//example.com/baz")
  #          |> to_string == "http://example.com/baz"

  #   assert :iam_uri.merge("http://google.com/foo", "//example.com/.././bar/../../../baz")
  #          |> to_string == "http://example.com/baz"

  #   assert :iam_uri.merge("http://example.com", :iam_uri.parse("/foo"))
  #          |> to_string == "http://example.com/foo"

  #   assert :iam_uri.merge("http://example.com", :iam_uri.parse("/.././bar/../../../baz"))
  #          |> to_string == "http://example.com/baz"

  #   base = :iam_uri.parse("http://example.com/foo/bar")
  #   assert :iam_uri.merge(base, "") |> to_string == "http://example.com/foo/bar"
  #   assert :iam_uri.merge(base, "#fragment") |> to_string == "http://example.com/foo/bar#fragment"
  #   assert :iam_uri.merge(base, "?query") |> to_string == "http://example.com/foo/bar?query"
  #   assert :iam_uri.merge(base, %URI{path: ""}) |> to_string == "http://example.com/foo/bar"

  #   assert :iam_uri.merge(base, %URI{path: "", fragment: "fragment"})
  #          |> to_string == "http://example.com/foo/bar#fragment"

  #   base = :iam_uri.parse("http://example.com")
  #   assert :iam_uri.merge(base, "/foo") |> to_string == "http://example.com/foo"
  #   assert :iam_uri.merge(base, "foo") |> to_string == "http://example.com/foo"

  #   base = :iam_uri.parse("http://example.com/foo/bar")
  #   assert :iam_uri.merge(base, "/baz") |> to_string == "http://example.com/baz"
  #   assert :iam_uri.merge(base, "baz") |> to_string == "http://example.com/foo/baz"
  #   assert :iam_uri.merge(base, "../baz") |> to_string == "http://example.com/baz"
  #   assert :iam_uri.merge(base, ".././baz") |> to_string == "http://example.com/baz"
  #   assert :iam_uri.merge(base, "./baz") |> to_string == "http://example.com/foo/baz"
  #   assert :iam_uri.merge(base, "bar/./baz") |> to_string == "http://example.com/foo/bar/baz"

  #   base = :iam_uri.parse("http://example.com/foo/bar/")
  #   assert :iam_uri.merge(base, "/baz") |> to_string == "http://example.com/baz"
  #   assert :iam_uri.merge(base, "baz") |> to_string == "http://example.com/foo/bar/baz"
  #   assert :iam_uri.merge(base, "../baz") |> to_string == "http://example.com/foo/baz"
  #   assert :iam_uri.merge(base, ".././baz") |> to_string == "http://example.com/foo/baz"
  #   assert :iam_uri.merge(base, "./baz") |> to_string == "http://example.com/foo/bar/baz"
  #   assert :iam_uri.merge(base, "bar/./baz") |> to_string == "http://example.com/foo/bar/bar/baz"

  #   base = :iam_uri.parse("http://example.com/foo/bar/baz")
  #   assert :iam_uri.merge(base, "../../foobar") |> to_string == "http://example.com/foobar"
  #   assert :iam_uri.merge(base, "../../../foobar") |> to_string == "http://example.com/foobar"
  #   assert :iam_uri.merge(base, "../../../../../../foobar") |> to_string == "http://example.com/foobar"

  #   base = :iam_uri.parse("http://example.com/foo/../bar")
  #   assert :iam_uri.merge(base, "baz") |> to_string == "http://example.com/baz"

  #   base = :iam_uri.parse("http://example.com/foo/./bar")
  #   assert :iam_uri.merge(base, "baz") |> to_string == "http://example.com/foo/baz"

  #   base = :iam_uri.parse("http://example.com/foo?query1")
  #   assert :iam_uri.merge(base, "?query2") |> to_string == "http://example.com/foo?query2"
  #   assert :iam_uri.merge(base, "") |> to_string == "http://example.com/foo?query1"

  #   base = :iam_uri.parse("http://example.com/foo#fragment1")
  #   assert :iam_uri.merge(base, "#fragment2") |> to_string == "http://example.com/foo#fragment2"
  #   assert :iam_uri.merge(base, "") |> to_string == "http://example.com/foo"
  # end
end