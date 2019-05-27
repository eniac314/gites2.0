defmodule GitesWeb.BackupsView do
  use GitesWeb, :view

  def render("list_backups.json", %{content: output}) do
    output
  end

  def render("export_backup.json", %{content: output}) do
    output
  end

  def render("success.json", _opts) do
    %{message: "success"}
  end
end
