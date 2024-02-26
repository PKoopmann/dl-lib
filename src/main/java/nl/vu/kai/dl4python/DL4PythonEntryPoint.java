package nl.vu.kai.dl4python;

import nl.vu.kai.dl4python.datatypes.DLHelpers$;
import nl.vu.kai.dl4python.datatypes.Ontology;
import nl.vu.kai.dl4python.owlapi.OWLParser;
import nl.vu.kai.dl4python.owlapi.OWLExporter;
import nl.vu.kai.dl4python.owlapi.OWLApiConverter;
import nl.vu.kai.dl4python.formatting.SimpleDLFormatterCl;
import nl.vu.kai.dl4python.reasoning.DLReasoner;
import nl.vu.kai.dl4python.reasoning.DLReasoners;
import py4j.GatewayServer;
import scala.Option;

public class DL4PythonEntryPoint {

    public DL4PythonEntryPoint() {
    }

    public OWLParser getOWLParser() {
        return new OWLParser();
    }


    public OWLParser getOWLParser(boolean simplifiedNames) {
        return new OWLParser(new OWLApiConverter(simplifiedNames, Option.empty()));
    }

    public void convertToBinaryConjunctions(Ontology ontology) {
        DLHelpers$.MODULE$.convert2binaryConjunctions(ontology);
    }

    public OWLExporter getOWLExporter() { return new OWLExporter(false); }

    public SimpleDLFormatterCl getSimpleDLFormatter() { return new SimpleDLFormatterCl(true); }

    public ELFactory$ getELFactory() {
        return ELFactory$.MODULE$;
    }

    public DLFactory$ getDLFactory() { return DLFactory$.MODULE$; }

    public String getType(Object object) {
        return object.getClass().getSimpleName();
    }

    public DLReasoner getELKReasoner() { return DLReasoners.getELKReasoner(); }

    public DLReasoner getHermiTReasoner() { return DLReasoners.getHermiTReasoner(); }

    public static void main(String[] args) {
        int portNumber = -1;
        if(args.length>0){
            portNumber = Integer.parseInt(args[0]);
            System.out.println("Trying port number "+portNumber);
        }

        GatewayServer gatewayServer;
        if(portNumber==-1)
            gatewayServer = new GatewayServer(new nl.vu.kai.dl4python.DL4PythonEntryPoint());
        else
            gatewayServer = new GatewayServer(new nl.vu.kai.dl4python.DL4PythonEntryPoint(), portNumber);
        gatewayServer.start();
        System.out.println("Gateway server for DL4Python started");
    }
}
