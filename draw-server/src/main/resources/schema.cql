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

CREATE TABLE IF NOT EXISTS draw.userSessions (
    sessionId uuid,
    userId uuid,
    expiration timestamp,
    PRIMARY KEY ((sessionId)));

CREATE TABLE IF NOT EXISTS draw.users (
    userId uuid,
    nickname text,
    PRIMARY KEY ((userId)));

CREATE TABLE IF NOT EXISTS draw.githubUser (
    githubId bigint,
    userId uuid,
    PRIMARY KEY ((githubId)));

