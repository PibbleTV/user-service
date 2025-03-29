package com.pibbletv.user_service.controller;

import com.pibbletv.user_service.business.interfaces.UserService;
import lombok.AllArgsConstructor;
import com.pibbletv.user_service.domain.User;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {

    private final UserService userService;

    @PostMapping(value = "/saveUser")
    public ResponseEntity<Void> saveUser(@RequestHeader("Authorization") String authorizationHeader) {
        String token = authorizationHeader.replace("Bearer ", "");
        userService.saveUser(token);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/getUser")
    public ResponseEntity<User> getUser(@RequestHeader("Authorization") String authorizationHeader) {

        String token = authorizationHeader.replace("Bearer ", "");
        return ResponseEntity.ok(userService.getUser(token));
    }
}
