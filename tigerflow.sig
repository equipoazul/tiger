signature tigerflow =
sig

    type instrTable = ((tigergraph.node, tigerassem.instr) tigertab.Tabla) ref
    type tempTable = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    type boolTable = (tigergraph.node, bool) tigertab.Tabla

    datatype flowgraph =
        FGRAPH of {control: tigergraph.graph,
                   def: tempTable ref,
                   use: tempTable ref,
                   ismove: boolTable ref}

    val newFlowGraph: unit -> flowgraph
    val instrs2graph: tigerassem.instr list -> (flowgraph * instrTable) 
    (*val instrs2graph: tigerassem.instr list -> flowgraph * tigergraph.node list*)
    (*val getNode: tigergraph.node -> tigerassem.instr *)
    (*val getLabelNode: (tigertemp.label * tigergraph.node) list -> tigertemp.label -> tigergraph.node*)
    val printGraphFlow: flowgraph -> unit

end
