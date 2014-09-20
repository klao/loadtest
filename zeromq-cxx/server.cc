#include <cstdio>
#include <unistd.h>
#include <zmq.hpp>

int main() {
  zmq::context_t context(1);
  zmq::socket_t socket(context, ZMQ_REP);
  { int ipv6 = 1; socket.setsockopt(ZMQ_IPV6, &ipv6, 4); }
  socket.bind("tcp://*:5500");

  for (int k = 1; ; ++k) {
    zmq::message_t req;
    socket.recv(&req);
    // printf(".");
    // if (k % 40 == 0) printf("\n");
    // fflush(stdout);

    // Work
    //usleep(200000);

    zmq::message_t resp(3);
    memcpy(resp.data(), "YYY", 3);
    socket.send(resp);
  }
}
