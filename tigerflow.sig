signature tigerflow =
sig

    type instrTable = (tigerassem.instr, tigergraph.node) tigertab.Tabla
    type tempTable = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    type boolTable = (tigergraph.node, bool) tigertab.Tabla

    datatype flowgraph =
        FGRAPH of {control: tigergraph.graph,
                   def: tempTable,
                   use: tempTable,
                   ismove: boolTable }

    val instrs2graph: tigerassem.instr list -> flowgraph * tigergraph.node list
    val fg : flowgraph
    val getNode: tigerassem.instr -> tigergraph.node
    val getLabelNode: tigertemp.label -> tigergraph.node
end

