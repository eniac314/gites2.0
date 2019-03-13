defmodule Gites.PagesData do
  @moduledoc """
  The PagesData context.
  """

  import Ecto.Query, warn: false
  alias Gites.Repo

  require Logger

  alias Gites.PagesData.PageData

  @doc """
  Returns the list of pagesdata.

  ## Examples

      iex> list_pagesdata()
      [%PageData{}, ...]

  """
  def list_pagesdata do
    Repo.all(PageData)
  end

  @doc """
  Gets a single page_data.

  Raises `Ecto.NoResultsError` if the Page data does not exist.

  ## Examples

      iex> get_page_data!(123)
      %PageData{}

      iex> get_page_data!(456)
      ** (Ecto.NoResultsError)

  """
  def get_page_data!(id), do: Repo.get!(PageData, id)

  @doc """
  Creates a page_data.

  ## Examples

      iex> create_page_data(%{field: value})
      {:ok, %PageData{}}

      iex> create_page_data(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_page_data(attrs \\ %{}) do
    
    Logger.debug("content: #{attrs}")

    %PageData{}
    |> PageData.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a page_data.

  ## Examples

      iex> update_page_data(page_data, %{field: new_value})
      {:ok, %PageData{}}

      iex> update_page_data(page_data, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_page_data(%PageData{} = page_data, attrs) do
    page_data
    |> PageData.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a PageData.

  ## Examples

      iex> delete_page_data(page_data)
      {:ok, %PageData{}}

      iex> delete_page_data(page_data)
      {:error, %Ecto.Changeset{}}

  """
  def delete_page_data(%PageData{} = page_data) do
    Repo.delete(page_data)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking page_data changes.

  ## Examples

      iex> change_page_data(page_data)
      %Ecto.Changeset{source: %PageData{}}

  """
  def change_page_data(%PageData{} = page_data) do
    PageData.changeset(page_data, %{})
  end
end
