signature tigerflow =
sig

    type instrTable = (tigergraph.node, tigerassem.instr) tigertab.Tabla
    type tempTable = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    type boolTable = (tigergraph.node, bool) tigertab.Tabla

    datatype flowgraph =
        FGRAPH of {control: tigergraph.graph,
                   def: tempTable ref,
                   use: tempTable ref,
                   ismove: boolTable ref}

    val instrs2graph: tigerassem.instr list -> flowgraph * tigergraph.node list
    val getNode: tigergraph.node -> tigerassem.instr 
    val getLabelNode: (tigertemp.label * tigergraph.node) list -> tigertemp.label -> tigergraph.node
    val printGraphFlow: flowgraph -> unit 
    
    val iTable : instrTable ref
    val defTable : tempTable ref
end

