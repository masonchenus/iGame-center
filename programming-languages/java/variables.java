// Variables in Java
public class Variables {
    public static void main(String[] args) {
        // Primitive types
        int age = 25;
        float price = 19.99f;
        double exactPrice = 19.99;
        char letter = 'A';
        boolean isActive = true;
        byte smallNumber = 127;
        short shortNumber = 32000;
        long bigNumber = 1000000L;
        
        // String (reference type)
        String name = "John";
        
        // Type inference (Java 10+)
        var dynamicVar = "I can be any type";
        
        // Constants
        final double PI = 3.14159;
        final int MAX_USERS = 100;
        
        System.out.println("Age: " + age);
        System.out.println("Price: " + price);
        System.out.println("Char: " + letter);
        System.out.println("Active: " + isActive);
        System.out.println("Name: " + name);
        System.out.println("PI: " + PI);
    }
}

