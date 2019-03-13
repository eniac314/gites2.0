defmodule GitesWeb.PageDataView do
  use GitesWeb, :view
  alias GitesWeb.PageDataView

  def render("index.json", %{pagesdata: pagesdata}) do
    %{data: render_many(pagesdata, PageDataView, "page_data.json")}
  end

  def render("show.json", %{page_data: page_data}) do
    %{data: render_one(page_data, PageDataView, "page_data.json")}
  end

  def render("page_data.json", %{page_data: page_data}) do
    %{id: page_data.id,
      name: page_data.name,
      content: page_data.content}
  end
end
