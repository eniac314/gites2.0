defmodule GitesWeb.AwsUploadController do
  use GitesWeb, :controller

  def get_url(conn, %{"id" => id}) do
    render conn, "presigned_url.json", presigned_s3_url: presigned_s3_url(id)
  end

  defp presigned_s3_url(id) do
    bucket = System.get_env("S3_BUCKET")
    path = "images/#{id}"
    {:ok, url} = ExAws.S3.presigned_url(ExAws.Config.new(:s3), :put, bucket, path, query_params: [{"x-amz-acl", "authenticated-read"}, {"content-type", "image/png"}])
    url
  end

end