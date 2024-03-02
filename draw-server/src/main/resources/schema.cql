CREATE KEYSPACE IF NOT EXISTS draw
  WITH REPLICATION = { 
   'class' : 'SimpleStrategy', 
   'replication_factor' : 1 
  };

CREATE TABLE IF NOT EXISTS draw.drawingEvents (
    drawingId uuid,  
    sequenceNr bigint,
    event blob, 
    PRIMARY KEY ((drawingId),sequenceNr));
