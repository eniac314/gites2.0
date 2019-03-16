defmodule GitesWeb.PageDataController do
  use GitesWeb, :controller
  
    plug Guardian.Plug.EnsureAuthenticated when action in [:create]


  alias Gites.PagesData
  alias Gites.PagesData.PageData

  action_fallback GitesWeb.FallbackController

  def index(conn, _params) do
    pagesdata = PagesData.list_pagesdata()
    render(conn, "index.json", pagesdata: pagesdata)
  end

  def create(conn, %{"name" => name, "content" => content}) do
    
    serialized_content = Poison.encode!(content)

    with {:ok, %PageData{} = page_data} <- PagesData.create_page_data(%{"name" => name, "content" => serialized_content}) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.page_data_path(conn, :show, page_data))
      |> render("show.json", page_data: page_data)
    end
  end

  def show(conn, %{"id" => id}) do
    page_data = PagesData.get_page_data!(id)
    unserialized_data = %{ page_data | content: Poison.decode!(page_data.content)}
    render(conn, "show.json", page_data: unserialized_data)
  end

  def update(conn, %{"id" => id, "page_data" => page_data_params}) do
    page_data = PagesData.get_page_data!(id)

    with {:ok, %PageData{} = page_data} <- PagesData.update_page_data(page_data, page_data_params) do
      render(conn, "show.json", page_data: page_data)
    end
  end

  def delete(conn, %{"id" => id}) do
    page_data = PagesData.get_page_data!(id)

    with {:ok, %PageData{}} <- PagesData.delete_page_data(page_data) do
      send_resp(conn, :no_content, "")
    end
  end
end
