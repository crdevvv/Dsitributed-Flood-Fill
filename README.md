Implementation of Distributed Flood Fill algorithm. 

The file that contains the graph must have the following format.</p> 
<ul>
   <li>The 1st line will consist of four terms separeted by a white space.
     <ul>
       <li>1st Term: Positive Integer N that denotes the number of vertices</li>
       <li>2nd Term: Positive Integer M that denotes the number of edges.</li>
       <li>3rd Term: Atom <code>directed</code> or <code>undirected</code> that denotes the type of the graph.</li>
       <li>4th Term: Atom <code>unweighted</code> or <code>d</code> or <code>f</code> that denotes the type of the edge weights.
         <ul>
           <li><code>unweighted</code> is for an unweighted graph.</li>
           <li><code>d</code> is for decimal integer weights.</li>
           <li><code>f</code> is for floating point number weights in proper Erlang syntax.</li>
         </ul>
       </li>
     </ul>
   </li>
   <li>The next M lines will consist of the edge descriptions. 
       Each line will contain three terms : U V W. 
       This will denote an edge from U to V with W weight (W applies only if the graph is weighted).</li>
 </ul>
 
 
