defmodule GitesWeb.AwsUploadController do
  use GitesWeb, :controller
  require Logger

  plug Guardian.Plug.EnsureAuthenticated when action in [:get_url]

  def index(conn, _params) do 
    bucket = System.get_env("S3_BUCKET")
    content = 
      ExAws.S3.list_objects(bucket) 
      |> ExAws.request!

    render conn, "bucket_content.json", content: content.body
  end 

  def delete(conn, %{"id" => id}) do 
    bucket = System.get_env("S3_BUCKET")
    path = "images/#{id}"
    res = 
      ExAws.S3.delete_object(bucket, path)
        |> ExAws.request!
    Logger.debug "Content: #{inspect(res)}"

    
    render conn, "success.json", %{}
  end 

  def get_url(conn, %{"id" => id}) do
    render conn, "presigned_url.json", presigned_s3_url: presigned_s3_url(id)
  end  

  defp presigned_s3_url(id) do
    bucket = System.get_env("S3_BUCKET")
    path = "images/#{id}"
    {:ok, url} = ExAws.S3.presigned_url(ExAws.Config.new(:s3), :put, bucket, path, query_params: [{"x-amz-acl", "authenticated-read"}, {"content-type", "image/png"}]) #,  {"ContentEncoding", "base64"}])
    url
  end

end