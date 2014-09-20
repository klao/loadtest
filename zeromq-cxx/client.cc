#include <cstdio>
#include <unistd.h>
#include <zmq.hpp>
#include "latency.hh"

int main() {
  zmq::context_t context(1);
  zmq::socket_t socket(context, ZMQ_REQ);
  socket.connect("tcp://localhost:5500");
  latency_t measure;

  usleep(100000);

  for (int k = 1; k <= 1000; ++k) {
    zmq::message_t req(2);
    memcpy(req.data(), "XX", 2);
    measure.start();
    socket.send(req);

    zmq::message_t resp;
    socket.recv(&resp);
    measure.stop();
    printf(".");
    if (k % 40 == 0) printf("\n");
    fflush(stdout);

    // klao: Something strange is going on here:
    // Without sleep the _mean_ is better than the _best case_ with sleep.
    // usleep(10000);
  }
  printf("\n");
  printf("mean: %.2f,  min: %.2f, 1%%: %.2f, 5%%: %.2f,     "
         "95%%: %.2f, 99%%: %.2f, max: %.2f\n", measure.mean_msec(),
         measure.minimum_msec(), measure.percentile_msec(1), measure.percentile_msec(5),
         measure.percentile_msec(95), measure.percentile_msec(99), measure.maximum_msec());
}
