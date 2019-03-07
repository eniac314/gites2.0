defmodule GitesWeb.AwsUploadView do
  use GitesWeb, :view
  alias GitesWeb.AwsUploadView

  def render("presigned_url.json", %{presigned_s3_url: url}) do
    %{presigned_s3_url: url}
  end

end
