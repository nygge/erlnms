%% moi = managed object instance generating the data, e.g. [{ne,host},{if,1}]
%% moc = managed object class, the class of object generating the data.
%% time = time stamp, date-time format, UTC
%% data = [counter]
-record(pm_rec,{moi,moc,time,data}).
