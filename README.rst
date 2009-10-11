CL-ZEROCONF
===========

Introduction
------------

CL-ZEROCONF is a Lisp interface to `Apple's open source
implementation`_ of the `Zeroconf`_ service discovery protocol, called
`Bonjour`_.  Apple does a good job of describing why Zeroconf is
useful:

  Bonjour lets you create an instant network of computers and smart
  devices just by getting them connected to each other.  The computers
  and devices take over from there, automatically broadcasting and
  discovering what services each is offering for the use of
  others. The network could be as simple as two !AirPort
  Extreme-equipped !PowerBook users sitting in a hotel meeting room
  miles from the nearest !AirPort Extreme Base Station with some large
  files they need to share. Before Bonjour, frustration. With Bonjour,
  your computer will discover others, making file sharing completely
  simple.

Examples
--------

Here's an example of advertising a service with CL-ZEROCONF::

  (dns-sd:publish (make-instance 'dns-sd:service
                                 :name "My CLiki"
                                 :type "_http._tcp"
                                 :port 80)
                  nil)

  (dns-sd:process-dns-sd-events 3.0)

Here's an example of browsing for services on the local network::

  (defclass my-observer ()
    ())

  (defmethod dns-sd:browse-add-service ((self my-observer) service &key more-coming-p)
    (declare (ignore more-coming-p))
    (format T "~&Found service ~S." service))

  ;; Look for iTunes servers.
  (let ((browser (dns-sd:browse "_daap._tcp" nil (make-instance 'my-observer))))
    (dotimes (i 10)
      (dns-sd:process-dns-sd-events 3.0))
    (dns-sd:cancel browser))

(For an explanation of the meanings of the _http._tcp and _daap._tcp
strings, see `DNS SRV (RFC 2782) Service Types`_.)

For some other short examples of how to use the library, see
`examples.lisp`_.  A small example application can be found in
`test-browse.lisp`_, which generates an `Araneida`_-based web page
that displays services on the local network:

.. image: http://github.com/wiseman/cl-zeroconf/raw/master/images/test-browse-s.jpg

Richard Newman used CL-ZEROCONF to create a `simple web interface to
iTunes`_.

Apple's `DNSServiceDiscovery API docs`_ may be helpful in
understanding how to use CL-ZEROCONF, as might their `Bonjour Network
Services`_ pages.

Availability and Implementation Support
---------------------------------------

You can download CL-ZEROCONF manually:
http://lemonodor.com/code/cl-zeroconf_0.2.tar.gz

Or you can use ASDF-INSTALL: ``(asdf-install:install '#:cl-zeroconf)``

CL-ZEROCONF has been tested under OS X 10.4.3 with OpenMCL 1.0, ACL
7.0, and !LispWorks Personal 4.3.0.  Under Linux, it has been tested
with ACL 7.0 (but see the notes below for ACL).  SBCL's support for
foreign callbacks is incomplete, and it therefore will not work with
CL-ZEROCONF.

See ReleaseNotes for more information.

Notes
-----

Even though it's named CL-ZEROCONF, this library only deals with one
part of Zeroconf: service discovery.  Link-local addressing
(allocation of IP addresses without a DHCP server) is outside its
scope, and probably doesn't make much sense for a Lisp library.

CL-ZEROCONF is really just a `UFFI`_-based wrapper around Apple's
multicast DNS Responder, which is a daemon that handles most of the
work in doing service discovery for you.  mDNSResponder is built in to
OS X, and is offered as `open source`_ for other operating systems.

It `took me`_ a `while`_ to come to mDNSResponder.  I tried `Howl`_,
but it was buggy, the API was incomplete and the event handling model
was different in OS X than in the other OSes it supports.  I tried the
Cocoa NSNetService API and the Core Foundation CFNetServices API, but
they weren't portable.  I considered writing a complete Lisp
implementation of multicast DNS that didn't rely on any external
libraries other than for basic networking, but that has some
disadvantages too.  mDNSResponder is free and quite portable, and
saved me a lot of work.

In order to use CL-ZEROCONF on Linux, you will need to download and
install mDNSResponder.  It can be `downloaded`_ from Apple] as a
tarball, or via CVS.  Make sure to `read the instructions`_ on using
Apple's Darwin CVS repository--you will need to get a (free) Apple ID
first.  The mDNSResponder code base is under active development, so I
recommend getting the code from CVS.  If you run into any trouble, the
tagged version I used was ``mDNSResponder-86``.  Once you've compiled
the code, install it and start the ``mdnsd`` daemon.

Coincidentally, both SBCL and ACL seem to have had an identical bug in
their handling of foreign ``short``s on PowerPC.  Franz recently fixed
the bug in patch update/paa005.001, which you can automatically
download and install (along with many other patches) by doing
``(sys:update-allegro)``.  The bug exhibits itself in CL-ZEROCONF by
causing all service port numbers to be reported as 0.

Once you've patched up your compilers and compiled new daemons, you
can install CL-ZEROCONF with ASDF-INSTALL (of course).

I hope you find this useful.

.. _Apple's open source implementation: http://developer.apple.com/opensource/internet/bonjour.html
.. _Zeroconf: http://zeroconf.org/
.. _Bonjour: http://www.apple.com/macosx/features/bonjour/
.. _DNS SRV (RFC 2782) Service Types: http://www.dns-sd.org/ServiceTypes.html
.. _examples.lisp: http://github.com/wiseman/cl-zeroconf/blob/master/examples.lisp
.. _test-browse.lisp: http://github.com/wiseman/cl-zeroconf/blob/master/test-browse.lisp
.. _Araneida: http://www.cliki.net/araneida
.. _simple web interface to iTunes: http://www.holygoat.co.uk/blog/entry/2005-02-11-2
.. _DNSServiceDiscovery API docs: http://developer.apple.com/mac/library/documentation/Networking/Conceptual/dns_discovery_api/Introduction.html
.. _Bonjour Network Services: http://developer.apple.com/documentation/Cocoa/Conceptual/NetServices/index.html#//apple_ref/doc/uid/10000119i
.. _UFFI: http://uffi.b9.com/
.. _open source: http://developer.apple.com/darwin/projects/bonjour/
.. _took me: http://lemonodor.com/archives/000685.html
.. _while: http://lemonodor.com/archives/000986.html
.. _Howl: http://www.porchdogsoft.com/products/howl
.. _downloaded: http://developer.apple.com/darwin/projects/bonjour/
.. _read the instructions: http://developer.apple.com/darwin/tools/cvs/howto.html
