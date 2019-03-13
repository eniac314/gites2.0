defmodule GitesWeb.PageDataControllerTest do
  use GitesWeb.ConnCase

  alias Gites.PagesData
  alias Gites.PagesData.PageData

  @create_attrs %{
    content: "some content",
    name: "some name"
  }
  @update_attrs %{
    content: "some updated content",
    name: "some updated name"
  }
  @invalid_attrs %{content: nil, name: nil}

  def fixture(:page_data) do
    {:ok, page_data} = PagesData.create_page_data(@create_attrs)
    page_data
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all pagesdata", %{conn: conn} do
      conn = get(conn, Routes.page_data_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create page_data" do
    test "renders page_data when data is valid", %{conn: conn} do
      conn = post(conn, Routes.page_data_path(conn, :create), page_data: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.page_data_path(conn, :show, id))

      assert %{
               "id" => id,
               "content" => "some content",
               "name" => "some name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.page_data_path(conn, :create), page_data: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update page_data" do
    setup [:create_page_data]

    test "renders page_data when data is valid", %{conn: conn, page_data: %PageData{id: id} = page_data} do
      conn = put(conn, Routes.page_data_path(conn, :update, page_data), page_data: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.page_data_path(conn, :show, id))

      assert %{
               "id" => id,
               "content" => "some updated content",
               "name" => "some updated name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, page_data: page_data} do
      conn = put(conn, Routes.page_data_path(conn, :update, page_data), page_data: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete page_data" do
    setup [:create_page_data]

    test "deletes chosen page_data", %{conn: conn, page_data: page_data} do
      conn = delete(conn, Routes.page_data_path(conn, :delete, page_data))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.page_data_path(conn, :show, page_data))
      end
    end
  end

  defp create_page_data(_) do
    page_data = fixture(:page_data)
    {:ok, page_data: page_data}
  end
end
