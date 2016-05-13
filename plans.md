# Plans
* write a new implementation
  * a graph is just a IxSet of nodes and edges are expressed as relations in
    keys
  * Nodes can contain graphs (second constructor)
  * we can induce graphs on subsets of nodes, keeping edges (as both keys stay)
  * and query our set by constructed primitive graphs, then we can reduce all
    occurances of such a structure!
    * In essence, we look at our graph from the outside

# DREAM Overview
* DFS over CFG nodes
  * find backedges to identify cyclic regions
* post-order visiting of the DFS tree
  * try to structure the current region (much like Iridium did)
    * head of acyclic region? <== not a target of backedge (?)
      * yes: compute dominated nodes, if the corresponding region (set?) 
        of nodes has a single successor structure region.
      * no, cyclic region: compute loop nodes (how? backedge search?)
        * multiple entries/successors? transform.
        * structure region.
  * reduce to abstract node if successful (when is that? how do we
    notice failure?)
* done? we have an AST

* structuring:
  * needs a single-entry,single-successor region
  * compute lexical order (what's that exactly? how do we do it?)
    * apparently postorder again to have all non-backedges point
      in one direction
  * compute header-relative reaching conditions for all nodes
  * acyclic:
    * group the nodes iteratively by condition to form if's and switches
      * find pairs with complementary conditions
        (see paper for details for now)
    * [ ] how do we find out what is code? ;)
  * cyclic:
    * first assume every edge to the successor node is a break.
    * structure the loop body as an acyclic region (recursive process)
    * decide upon loop type and find condition (essentially transformation)
      * [ ] we probably could merge it with a secondary transformation step
      * pretty close to Iridium as well

* transforming:
  * compute exact conditions for structure entry from a given edge/node
  * transform structure based on some simple rules (see images in paper)

* [ ] find a definition of graph regions
