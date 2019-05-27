defmodule GitesWeb.BackupsController do
  use GitesWeb, :controller

  alias Gites.BackupServer

  # plug(
  #   Guardian.Plug.EnsureAuthenticated
  #   when action in [
  #          :list_backups,
  #          :import_backup,
  #          :manual_backup,
  #          :export_backup,
  #          :restore_backup,
  #          :delete_backups
  #        ]
  # )

  def list_backups(conn, _params) do
    content = BackupServer.list_backups()
    render(conn, "list_backups.json", content: content)
  end

  def import_backup(conn, %{"payload" => payload}) do
    BackupServer.external_manual_backup(Base.decode64(payload))
    render(conn, "success.json", %{})
  end

  def manual_backup(conn, _params) do
    BackupServer.manual_backup()
    render(conn, "success.json")
  end

  def export_backup(conn, %{"key" => key}) do
    BackupServer.load_backup("Backups/" <> key)
    %{key: key, payload: payload} = BackupServer.export_backup()

    render(conn, "export_backup.json", content: %{key: key, payload: Base.encode64(payload)})
  end

  def restore_backup(conn, %{"key => key"}) do 
  	BackupServer.load_backup("Backups/" <> key)
  	BackupServer.restore_backup()
  	render(conn, "success.json")
  end 
end
