defmodule GitesWeb.AwsUploadController do
  use GitesWeb, :controller
  require Logger

  plug(
    Guardian.Plug.EnsureAuthenticated
    when action in [:index, :get_url, :delete, :delete_folder, :show]
  )

  def index(conn, _params) do
    bucket = System.get_env("S3_BUCKET")

    objects =
      ExAws.S3.list_objects(bucket)
      |> ExAws.request!()

    heads = get_metadata(bucket, objects)

    render(conn, "bucket_content.json", content: heads)
  end

  def show(conn, %{"folder" => folder}) do
    bucket = System.get_env("S3_BUCKET")

    objects =
      ExAws.S3.list_objects(bucket, delimiter: "/", prefix: folder <> "/")
      |> ExAws.request!()

    heads = get_metadata(bucket, objects)

    render(conn, "bucket_content.json", content: heads)
  end

  defp get_metadata(bucket, objects) do
    objects.body.contents
    |> Enum.map(fn o -> {o.key, ExAws.S3.head_object(bucket, o.key)} end)
    |> Enum.map(fn {k, r} -> {k, ExAws.request!(r)} end)
    |> Enum.map(fn {k, h} ->
      {k, Map.new(Map.get(h, :headers))}
    end)
    |> Enum.map(fn {k, header} ->
      {k,
       %{
         width: Map.get(header, "x-amz-meta-width", "0"),
         height: Map.get(header, "x-amz-meta-height", "0")
       }}
    end)
    |> Map.new()
  end

  def delete(conn, %{"filename" => safe_id}) do
    bucket = System.get_env("S3_BUCKET")
    path = String.replace(String.replace(safe_id, "¤", "/"), "_", " ")

    ExAws.S3.delete_object(bucket, path)
    |> ExAws.request()

    case String.split(path, "/") |> Enum.reverse() do
      [] ->
        nil

      [filename | sub_path] ->
        Enum.reverse([filename | ["thumbs" | sub_path]])
        |> Enum.join("/")
        |> (fn path -> ExAws.S3.delete_object(bucket, path) end).()
        |> ExAws.request!()
    end

    render(conn, "success.json", %{})
  end

  def delete_folder(conn, %{"folder" => folder}) do
    bucket = System.get_env("S3_BUCKET")

    objects =
      ExAws.S3.list_objects(bucket, prefix: folder <> "/")
      |> ExAws.request!()
      |> (fn obs -> obs.body.contents end).()
      |> Enum.map(fn o -> o.key end)

    ExAws.S3.delete_all_objects(bucket, objects)
    |> ExAws.request!()

    render(conn, "success.json", %{})
  end

  def get_url(conn, %{"mime" => mime, "filename" => safe_id, "metadata" => meta}) do
    id = String.replace(String.replace(safe_id, "¤", "/"), "_", " ")
    render(conn, "presigned_url.json", presigned_s3_url: presigned_s3_url(id, mime, meta))
  end

  defp presigned_s3_url(id, mime, meta) do
    bucket = System.get_env("S3_BUCKET")
    path = "#{id}"

    query_params = [
      {"x-amz-acl", "authenticated-read"},
      {"content-type", mime},
      {"x-amz-meta-width", meta["width"]},
      {"x-amz-meta-height", meta["height"]}
    ]

    {:ok, url} =
      ExAws.S3.presigned_url(ExAws.Config.new(:s3), :put, bucket, path, query_params: query_params)

    url
  end
end
