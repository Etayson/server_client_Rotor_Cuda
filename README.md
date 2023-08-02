# server_client_Rotor_Cuda
Server and client app for Rotor-Cuda solver ( https://github.com/Vladimir855/Rotor-Cuda ), you need to have solver in the same client folder.
## [serverRotorCudaX64.exe] commands:
```
-port N           Listen port N for client connections
-range begin:end  Using range from begin to end
-dp N             Divide whole range into 2^N subranges
-out fileneame    Name of your log file where will be stored finded key
-address ADDR     Address that will be find with bitckrack app.
-map fileneame    Name of your map file where will be stored subranges key
Example: serverRotorCudaX64.exe -range 8000000000:ffffffffff -dp 5 -map mmm.bin -address 1EeAxcprB2PpCnr34VfZdFrkUWuxyiNEFv
```

## [clientRotorCudaX64.exe] commands:
```
-prog filename    The name of solver app, default Rotor-Cuda.exe
-name  NAME       Instance name for stats on server
-pool host:port   Server host:port by default 127.0.0.1:8000
-d N              GPU device id, coma separated
-b b,t            Number of blocks and threads per GPU
-nopow            Searching without pow adress, not recomended.            
Example: clientRotorCudaX64.exe -prog Rotor-Cuda.exe -d 0 -name 1660s -pass x -pool 127.0.0.1:8000 -b 132,256
or
clientRotorCudaX64.exe -prog Rotor-Cuda.exe -d 0 -name 1660s -pass x -pool 127.0.0.1:8000 -b 132,256 -nopow
```

Purebasic v.5.31 required for compilation.  
Note! When you complile apps by yourself don`t forget in compile options:
<ul>
  <li>Uncheck > create unicode executable</li>
  <li>Check > Create threadsafe executable</li>
  <li>Executable format> console</li>
</ul>

Note! Rotor-Cuda app should be put in the same folder as clientRotorCudaX64.exe  
Job timeout is 1 day. If client do not submit job in 1 day, job will be deleted.  
So the width of the subrange should be such that it can be solved in 1 day maximum by 1 instance client.  
For ex your gpu can calculate 2^30 keys/s  
In this case for 2.5h  gpu can calculate 2^43keys  
If your whole range is 2^63 than devide 2^63 / 2^43 = 2^20  
So -dp 20, mean devide whole range into 1048576(2^20) subranges and width of each subrange is 2^43.  
