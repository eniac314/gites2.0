defmodule Gites.Email do
  import Bamboo.Email
  import Bamboo.Phoenix

  def confirm_email(dest) do
    base_email
    |> to(dest)
    |> subject("Welcome!!!")
    # |> put_header("Reply-To", "florian.gillard@tutanota.com")
    |> text_body("Welcome")
  end

  defp base_email do
    # Here you can set a default from, default headers, etc.
    Bamboo.Email.new_email
    |> from("gite@levieuxlilas.fr")
    # |> put_html_layout({MyApp.LayoutView, "email.html"})
    |> put_text_layout({Gites.LayoutView, "email.text"})
  end
end 