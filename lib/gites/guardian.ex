defmodule Gites.Guardian do
  use Guardian, otp_app: :gites
  
  alias Gites.Auth
  
  def subject_for_token(user, _claims) do
   {:ok, "User:#{user.id}"}
  end 
  
  def resource_from_claims(claims) do
    "User:" <> id = claims["sub"]
    {:ok, Auth.get_user!(id)} 
  end
end