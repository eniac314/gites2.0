defmodule Gites.LockedAvailabilitiesServer do 
  use GenServer 

  @name :avail_lock_server
  
  def start_link(_init_state) do 
  	IO.puts "Starting the AvailLock server..."
  	GenServer.start_link(__MODULE__, %{}, name: @name)
  end 

  def list_locked do 
  	GenServer.call @name, :list_locked
  end

  def new_entry({uuid, cIn, cOut}) do 
  	GenServer.cast @name, {:new_entry, uuid, cIn, cOut} 
  end  

  def refresh(current_users) do 
  	GenServer.cast @name, {:refresh, current_users}
  end 


  def handle_call(:list_locked, _from, state) do 
  	res = 
  		Enum.map(state, fn {k, {cIn, cOut}} -> %{uuid: k, cIn: cIn, cOut: cOut} end)
  	{:reply, res, state}
  end 

  def handle_cast({:new_entry, uuid, cIn, cOut}, state) do 
  	{:noreply, Map.put(state, uuid, {cIn, cOut})}
  end

  def handle_cast({:refresh, current_users}, state) do 
  	new_state = 
  		case Enum.filter(state, fn {uuid, _v} -> Enum.member?(current_users, uuid) end) do
  			[] -> %{}
  			s  -> s 
  		end 
  	{:noreply, new_state}
  end 

end 