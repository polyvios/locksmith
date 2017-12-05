# locksmith

Locksmith is a static analysis tool that tries to find data races in multithreaded C programs. To do that, it implements a static version of the Lockset algorithm. A race occurs whenever two threads access a memory location without any synchronization, and one of the accesses is a write. Locksmith analyzes multithreaded programs that use locks (or mutexes) to protect accesses to shared locations, and tries to discover which, if any, locks protect each shared location. Whenever an access to a shared memory location is not protected by any lock, it potentially is a race. 

See also [http://www.cs.umd.edu/projects/PL/locksmith/].
