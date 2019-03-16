defmodule Gites.AuthLock do 
  use GenServer 

  @name :auth_lock_server

  def start_link(_init_state) do 
  	IO.puts "Starting the AuthLock server..."
  	GenServer.start_link(__MODULE__, {:false, nil}, name: @name)
  end 

  def is_locked do 
  	GenServer.call @name, :is_locked
  end 

  def lock(token) do 
  	GenServer.cast @name, {:lock, token}
  end 

  def unlock do 
  	GenServer.cast @name, :unlock
  end


  def handle_call(:is_locked, _from, {state,token}) do 
  	{:reply, state, {state,token}}
  end 

  def handle_cast({:lock, token},  {state,_old_token}) do 
  	{:noreply, {:true, token}}
  end 

  def handle_cast(:unlock, {state,token}) do 
  	{:noreply, {:false, nil}}
  end 
end 
