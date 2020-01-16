defmodule GitesWeb.AwsUploadView do
  use GitesWeb, :view
  alias GitesWeb.AwsUploadView
  
  def render("bucket_content.json", %{content: content}) do
  	%{content: content}
  end 

  def render("presigned_url.json", %{presigned_s3_url: url}) do
    %{presigned_s3_url: url}
  end

  def render("success.json", _opts) do 
    %{message: "success"}
  end

end
