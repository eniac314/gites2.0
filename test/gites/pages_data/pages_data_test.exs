defmodule Gites.PagesDataTest do
  use Gites.DataCase

  alias Gites.PagesData

  describe "pagesdata" do
    alias Gites.PagesData.PageData

    @valid_attrs %{content: "some content", name: "some name"}
    @update_attrs %{content: "some updated content", name: "some updated name"}
    @invalid_attrs %{content: nil, name: nil}

    def page_data_fixture(attrs \\ %{}) do
      {:ok, page_data} =
        attrs
        |> Enum.into(@valid_attrs)
        |> PagesData.create_page_data()

      page_data
    end

    test "list_pagesdata/0 returns all pagesdata" do
      page_data = page_data_fixture()
      assert PagesData.list_pagesdata() == [page_data]
    end

    test "get_page_data!/1 returns the page_data with given id" do
      page_data = page_data_fixture()
      assert PagesData.get_page_data!(page_data.id) == page_data
    end

    test "create_page_data/1 with valid data creates a page_data" do
      assert {:ok, %PageData{} = page_data} = PagesData.create_page_data(@valid_attrs)
      assert page_data.content == "some content"
      assert page_data.name == "some name"
    end

    test "create_page_data/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = PagesData.create_page_data(@invalid_attrs)
    end

    test "update_page_data/2 with valid data updates the page_data" do
      page_data = page_data_fixture()
      assert {:ok, %PageData{} = page_data} = PagesData.update_page_data(page_data, @update_attrs)
      assert page_data.content == "some updated content"
      assert page_data.name == "some updated name"
    end

    test "update_page_data/2 with invalid data returns error changeset" do
      page_data = page_data_fixture()
      assert {:error, %Ecto.Changeset{}} = PagesData.update_page_data(page_data, @invalid_attrs)
      assert page_data == PagesData.get_page_data!(page_data.id)
    end

    test "delete_page_data/1 deletes the page_data" do
      page_data = page_data_fixture()
      assert {:ok, %PageData{}} = PagesData.delete_page_data(page_data)
      assert_raise Ecto.NoResultsError, fn -> PagesData.get_page_data!(page_data.id) end
    end

    test "change_page_data/1 returns a page_data changeset" do
      page_data = page_data_fixture()
      assert %Ecto.Changeset{} = PagesData.change_page_data(page_data)
    end
  end
end
