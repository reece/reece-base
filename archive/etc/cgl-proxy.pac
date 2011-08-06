// -*-javascript-*-

// in .ssh/config:
// Host cgl-proxy
// 	 Hostname socrates.cgl.ucsf.edu
// 	 User rkh
// 	 DynamicForward 1078


function FindProxyForURL(url, host) {
	p = _FindProxyForURL(url, host)
	alert("URL="+url+" HOST="+host+" => PROXY="+proxy); 
	return p
}

function _FindProxyForURL(url, host) {
	// direct if at UCSF...
	// not yet implemented
	//if (isInNet(myIpAddress(),"131.225.0.0","255.255.0.0")) {
	//    return "DIRECT";	/
    //}

	// tunnel everything for now, but this should change
  	return "PROXY localhost:1078; DIRECT";

    if (	   shExpMatch(url,"www.sciencedirect.com")
			// || shExpMatch(url,"bogusbogus")
			// ...
	    ) {
			return "SOCKS5 localhost:1078";
		}
                                                     
    return "DIRECT";
}
