package org.ants.verifier;

import java.io.IOException;
import java.util.*;

import org.ants.verifier.apkeep.checker.ReachabilityChecker;
import org.ants.verifier.apkeep.core.Network;
import org.ants.verifier.apkeep.utils.UtilityTools;
import org.ants.verifier.common.Utility;

public class DPVerifier {
    private final Network apkeepNetworkModel;
    private final ReachabilityChecker apkeepVerifier;
	private ArrayList<String> policies;

    public long dpm_time;
    public long dpv_time;

    public DPVerifier(String network_name, ArrayList<String> topo, ArrayList<String> edge_ports,
    		Map<String, Map<String, Object>> dpDevices) throws IOException{
    	apkeepNetworkModel = new Network(network_name);
    	apkeepNetworkModel.initializeNetwork(topo, edge_ports, dpDevices);

    	apkeepVerifier = new ReachabilityChecker(apkeepNetworkModel);
    	apkeepVerifier.initializeChecker(apkeepNetworkModel);

    	dpm_time=0;
    	dpv_time=0;
    }

    public ArrayList<String> run(ArrayList<String> forwarding_rules, Map<String, Map<String, Object>> dpDevices) throws IOException{
    	long t0 = System.nanoTime();
    	HashMap<String,HashSet<Integer>> moved_aps = apkeepNetworkModel.UpdateBatchRules(forwarding_rules, dpDevices, apkeepVerifier);
    	long t1 = System.nanoTime();
    	apkeepVerifier.updateReachability(moved_aps);
    	long t2 = System.nanoTime();
    	dpm_time+=(t1-t0);
    	dpv_time+=(t2-t1);
   		// System.out.println(apkeepNetworkModel.getAPNum()+" "+(t1-t0)/1000000+" "+(t2-t1)/1000000);

		policies = apkeepNetworkModel.getReachabilityChanges(apkeepVerifier.getChanges());
		return policies;
    }

	public ArrayList<String> getPolicies() {
		return policies;
	}

    public void dumpResults(String outputPath) throws IOException{
    	apkeepNetworkModel.writeReachabilityChanges(outputPath, apkeepVerifier.getChanges());
    }

	public void dumpReachability(String outputPath) throws IOException {
		apkeepNetworkModel.writeReachabilityMatrix(outputPath, apkeepVerifier.getReachabilityMatrix());
	}

    public boolean reach(String src, String dst) {
		Hashtable<String, Hashtable<String, HashSet<Integer>>> reach =
			apkeepVerifier.getReachabilityMatrix();
		Hashtable<String, HashSet<Integer>> src_reach = reach.get(src);
		if (src_reach == null) return false;
		HashSet<Integer> src_dst_ips = src_reach.get(dst);
		return src_dst_ips != null && !src_dst_ips.isEmpty();
	}

	public boolean checkConnected() {
		boolean ok = true;
		HashSet<String> hosts = apkeepNetworkModel.getEndHosts();
		for (String src : hosts) {
			for (String dst : hosts) {
				if(src.equals(dst) || src.contains(",") || dst.contains(",")) {
					continue;
				}
				ok = ok && reach(src,dst);
			}
		}
		return ok;
	}

	public static void fattree() {
		try {
			// Construct DPVerifier object
			String network_name = "bgp_fattree04";
			String dir = System.getProperty("user.dir");
			String topoFolder = dir + "/networks/" + network_name;
			String rulesFolder = topoFolder + "/" + network_name + "_lf";

			ArrayList<String> topo = UtilityTools.readFile(topoFolder + "/layer1Topology");
			ArrayList<String> edge_ports = UtilityTools.readFile(topoFolder + "/edgePorts");
			HashMap<String, Map<String, Object>> dp_devices = null; // OK, as this encodes ACL lists
			DPVerifier dpv = new DPVerifier(network_name, topo, edge_ports, dp_devices);

			ArrayList<String> fwd_rules = UtilityTools.readFile(rulesFolder + "/change_base");
			dpv.run(fwd_rules, dp_devices);
			dpv.dumpResults("fattree.txt");
		} catch (java.io.IOException exc) {
			System.out.println("Unexpected error: " + exc);
		}
	}

	public static void simple() {
		try {
			String network_name = "simple";
			ArrayList<String> topo = new ArrayList<String>(Arrays.asList(
				"s1 p2 s2 p1",
				"s2 p1 s1 p2"
			));
			ArrayList<String> edge_ports = new ArrayList<String>(Arrays.asList(
				"s1 p0",
				"s2 p0"
			));
			HashMap<String, Map<String, Object>> dp_devices = null;
			DPVerifier dpv = new DPVerifier(network_name, topo, edge_ports, dp_devices);
			ArrayList<String> fwd_rules = new ArrayList<String>(Arrays.asList(
				"+ fwd s1 167776768 32 p2 32",
				"+ fwd s2 167776768 32 p0 32"
			));
			dpv.run(fwd_rules, dp_devices);
			dpv.dumpResults("simple.txt");
			dpv.dumpReachability("simple-reach.txt");
		} catch (java.io.IOException exc) {
			System.out.println("Unexpected error: " + exc);
		}
	}

	// Uses APKeep to check connectivity for a network generated from the 5stars.
	// The following files should be in the same directory as where `make run` is invoked.
	// - topo.apk
	// - edges.apk
	// - routing.apk
	public static void fivestars() {
		try {
			// Construct DPVerifier object
			String network_name = "localdir";
			String dir = System.getProperty("user.dir");
			ArrayList<String> nets = new ArrayList<String>();
			// Determine all networks to be tested by iterating over the apk files
			for(String file : new java.io.File(dir + "/apk").list()) {
				if(file.endsWith("_topo.apk")) {
					String name = file.substring(0, file.length() - "_topo.apk".length());
					if(name == "Layer42" || name == "Compuserve" || name == "Airtel" || name == "Shentel" || name == "Sanet" || name == "Uunet" || name == "Telcove" || name == "Missouri" || name == "Deltacom" || name == "Cogentco" || name == "Kdl") {
						continue;
					}
					nets.add(file.substring(0, file.length() - "_topo.apk".length()));
				}
			}
			// nets.add("Layer42");
			// nets.add("Compuserve");
			// nets.add("Airtel");
			// nets.add("Shentel");
			// nets.add("Sanet");
			// nets.add("Uunet");
			// nets.add("Telcove");
			// nets.add("Missouri");
			// nets.add("Deltacom");
			// nets.add("Cogentco");
			// nets.add("Kdl");
			System.out.println(nets);
			StringBuilder sb = new StringBuilder();
			for(String net : nets) {
				double total_init_time = 0;
				double total_comp_time = 0;
				int runs = 0;
				int bench_runs = 100;
				int warmup_runs = 10;
				for(int i = 0; i < bench_runs + warmup_runs; i++){
					long startTime = System.currentTimeMillis();
					ArrayList<String> topo = UtilityTools.readFile(dir + "/apk/" + net + "_topo.apk");
					ArrayList<String> edge_ports = UtilityTools.readFile(dir + "/apk/" + net + "_hosts.apk");
					HashMap<String, Map<String, Object>> dp_devices = null; // OK, as this encodes ACL lists
					DPVerifier dpv = new DPVerifier(network_name, topo, edge_ports, dp_devices);
					ArrayList<String> fwd_rules = UtilityTools.readFile(dir + "/apk/" + net + "_routing.apk");
					long initTime = System.currentTimeMillis();
					dpv.run(fwd_rules, dp_devices);
					/*  Next two lines might be useful for debugging... */
					// dpv.dumpResults("apkeep.txt");
					// dpv.dumpReachability("apkeep-reach.txt");
					long endTime = System.currentTimeMillis();
					boolean connected = dpv.checkConnected();
					long init = initTime - startTime;
					long comp = endTime - initTime;
					System.out.println(net + " Init: " + init + "ms, Comp: " + comp + "ms (connected: " + connected + ")");
					if(!(i < warmup_runs)){
						total_init_time += (initTime - startTime);
						total_comp_time += (endTime - initTime);
						runs++;
						// if(endTime - startTime > 100) System.out.println(net + " Time: " + (endTime - startTime) + "ms   (connected: " + connected + ")");
					}
				}
				double avg_init = total_init_time / runs;
				double avg_comp = total_comp_time / runs;
				String msg = net + " & " + avg_init + " & " + avg_comp + " \\\\";
				System.out.println(msg);
				sb.append(msg + "\n");
			}
			System.out.println(sb.toString());
			// write the results to a file
			java.io.FileWriter fw = new java.io.FileWriter("apkeep_bench.txt");
			fw.write(sb.toString());
			fw.close();
		} catch (java.io.IOException exc) {
			System.out.println("Unexpected error: " + exc);
		}
	}

	public static void main(String[] args) {
		fivestars();
	}
}
