defmodule Gites.Email do
  import Bamboo.Email
  import Bamboo.Phoenix

  def notif_admin_email(subject, body) do
    base_email
    |> to(["florian.gillard@tutanota.com"])
    |> subject(subject)
    |> text_body(body)
  end
  
  def notif_email(dest, subject, body) do
    base_email
    |> to(dest)
    |> subject(subject)
    |> text_body(body)
  end

  defp base_email do
    # Here you can set a default from, default headers, etc.
    Bamboo.Email.new_email
    |> from("gite@levieuxlilas.fr")
    # |> put_html_layout({MyApp.LayoutView, "email.html"})
    |> put_text_layout({Gites.LayoutView, "email.text"})
  end
end 