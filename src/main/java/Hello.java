package main.java;

public class Hello {
    private final String name;

    public Hello(String name){
        this.name = name;
    }

    public String sayHello(){
        return "Hello, " + this.name +"!";
    }

}
