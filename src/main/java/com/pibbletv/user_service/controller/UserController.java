package com.pibbletv.user_service.controller;

import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.domain.User;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.Map;

import static jakarta.ws.rs.core.Response.ok;
import static org.springframework.http.ResponseEntity.badRequest;

@RestController
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {

    private final UserService userService;

    @PostMapping("/saveUser")
    public Mono<Void> saveUser(@RequestParam String username) {
        return userService.saveUser(username)
                .onErrorResume(RuntimeException.class, ex ->
                        Mono.error(new ResponseStatusException(
                                HttpStatus.BAD_REQUEST,
                                ex.getMessage()
                        ))
                );
    }

    @GetMapping(value = "/getUser")
    public Mono<User> getUser(@RequestParam String username) {
        return userService.getUser(username);
    }
}