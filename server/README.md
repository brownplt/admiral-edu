# Configuration

I am using Ubunto 12.04

- Racket 6.0.1
- Apache 2.2.22
- MySQL 5

To run type `racket server/captain-teach.rkt`. This starts a service on port 8080.

In your apache configuration file, you will want to setup a proxy to forward to the service. For example:

    ProxyPass / http://localhost:8080/
    ProxyPassReverse / http://localhost:8080/
