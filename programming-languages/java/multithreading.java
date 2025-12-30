import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

public class MultiThreadingExample {
    
    public static void main(String[] args) throws InterruptedException {
        // Using ExecutorService
        ExecutorService executor = Executors.newFixedThreadPool(4);
        
        List<Future<String>> futures = new ArrayList<>();
        
        for (int i = 1; i <= 10; i++) {
            final int taskId = i;
            Future<String> future = executor.submit(() -> {
                try {
                    Thread.sleep(1000);
                    return "Task " + taskId + " completed by " + Thread.currentThread().getName();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return "Task " + taskId + " was interrupted";
                }
            });
            futures.add(future);
        }
        
        // Collect results
        for (Future<String> future : futures) {
            try {
                System.out.println(future.get());
            } catch (ExecutionException e) {
                System.err.println("Task execution failed: " + e.getMessage());
            }
        }
        
        executor.shutdown();
    }
    
    // Using Atomic variables for thread-safe operations
    public static class Counter {
        private final AtomicInteger count = new AtomicInteger(0);
        
        public void increment() {
            count.incrementAndGet();
        }
        
        public int getCount() {
            return count.get();
        }
        
        public void reset() {
            count.set(0);
        }
    }
    
    // Producer-Consumer pattern
    public static class ProducerConsumerExample {
        private final BlockingQueue<String> queue = new ArrayBlockingQueue<>(10);
        
        public void start() throws InterruptedException {
            // Producer thread
            Thread producer = new Thread(() -> {
                try {
                    for (int i = 1; i <= 20; i++) {
                        queue.put("Item " + i);
                        System.out.println("Produced: Item " + i);
                        Thread.sleep(100);
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
            
            // Consumer thread
            Thread consumer = new Thread(() -> {
                try {
                    while (true) {
                        String item = queue.take();
                        System.out.println("Consumed: " + item);
                        Thread.sleep(200);
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
            
            producer.start();
            consumer.start();
            
            producer.join();
            consumer.interrupt();
        }
    }
}
